/* $NetBSD$ */

/*
 * File "newfs_udf.c" is part of the UDFclient toolkit.
 * File $Id: newfs_udf.c,v 1.46 2017/01/18 14:23:35 reinoud Exp $ $Name:  $
 *
 * Copyright (c) 2004, 2005, 2006, 2011
 * 	Reinoud Zandijk <reinoud@netbsd.org>
 * All rights reserved.
 *
 * The UDFclient toolkit is distributed under the Clarified Artistic Licence.
 * A copy of the licence is included in the distribution as
 * `LICENCE.clearified.artistic' and a copy of the licence can also be
 * requested at the GNU foundantion's website.
 *
 * Visit the UDFclient toolkit homepage http://www.13thmonkey.org/udftoolkit/
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <time.h>
#include <sys/time.h>
#include "udf.h"
#include "udf_bswap.h"


/* switches */

/* #define DEBUG(a) (a) */
#define DEBUG(a) if (0) { a; }


/* include the dump parts ... in order to get a more sane splitting */
extern void udf_dump_descriptor(union dscrptr *dscrpt);
extern void udf_dump_alive_sets(void);
extern void udf_dump_root_dir(struct udf_mountpoint *mountpoint);
extern void udf_dump_discinfo(struct udf_discinfo *disc);


/* globals */
extern int udf_verbose;
extern int uscsilib_verbose;


/* UDF library dependencies for writing */
int udf_add_session_to_discinfo(struct udf_discinfo *disc, int session, struct anchor_vdp *avdp, int error);
int udf_stop_writing_threads(struct udf_discinfo *disc);
int udf_get_volumeset_space(struct udf_discinfo *disc);


#ifndef MAX
#define MAX(a,b) ((a)>(b)?(a):(b))
#define MIN(a,b) ((a)<(b)?(a):(b))
#endif


/*
 * newfs
 *
 * Simple-UDF-disc-types recognized by this newfs program :
 */

#define DISC_TYPE_NORMAL    0
#define DISC_TYPE_VIRTUAL   1
#define DISC_TYPE_SPARABLE  2
#define DISC_TYPE_META      3



void newfs_test_callback(int reason, struct udf_wrcallback *wrcallback, int error, uint8_t *sectordata) {
#if 0
	DEBUG(
		printf("WRcallback called with sector data %p\n", sectordata);
		printf("\treason    %d\n", reason);
		printf("\toffset    %08d\n", (int) wrcallback->offset);
		printf("\tlb_num    %08d\n", (int) wrcallback->lb_num);
		printf("\tlength    %08d\n", (int) wrcallback->length);
		printf("\terror     %d\n",   (int) error);
	);
#else
	reason = reason;
	wrcallback = wrcallback;
	sectordata = sectordata;
#endif
	if (error) {
		fprintf(stderr, "HELP! got writing error while writing; can't fix yet! (%s)\n", strerror(error));
		return;
	}
}


void writeout_vds(struct udf_session *udf_session, uint16_t dscr_ver) {
	struct udf_wrcallback     wrcallback;
	struct vrs_desc          *vrs_desc;
	uint32_t pos, sector_size, dpos;
	uint32_t cnt;

	sector_size = udf_session->disc->sector_size;
	wrcallback.function = newfs_test_callback;

	/* Build ISO/Ecma-167 volume recognition sequence */
	vrs_desc = calloc(1, 64*1024); assert(vrs_desc);	/* working copy of one max sized ISO descriptor */

	/* white out VRS */
	pos = ((32*1024 + sector_size - 1) / sector_size);	/* definition: first sector AFTER 32kb, minimum sector size 2048 */
	dpos = (2048 + sector_size - 1) / sector_size;
	for (cnt = 0; cnt < 6*dpos; cnt++) {
		udf_write_session_sector(udf_session, pos + cnt, "blank VRS", (uint8_t *) vrs_desc, 0, &wrcallback);
	}

	/* write out VRS */
	vrs_desc->struct_type = 0;
	vrs_desc->version     = 1;

	pos  = ((32*1024 + sector_size - 1) / sector_size);	/* definition: first sector AFTER 32kb + descr*2048 */

#if 0
	/* CD001 identifies bridge disc with ISO 9660 */
	memcpy(vrs_desc->identifier, VRS_CD001, 5);
	udf_write_session_sector(udf_session, pos, "VRS CD001",    (uint8_t *) vrs_desc, 0, &wrcallback);
	pos += dpos;
#else
	memcpy(vrs_desc->identifier, VRS_BEA01, 5);
	udf_write_session_sector(udf_session, pos, "VRS BEA01",    (uint8_t *) vrs_desc, 0, &wrcallback);
	pos += dpos;
#endif
	if (dscr_ver == 2) {
		memcpy(vrs_desc->identifier, VRS_NSR02, 5);
	} else {
		memcpy(vrs_desc->identifier, VRS_NSR03, 5);
	}
	udf_write_session_sector(udf_session, pos, "VRS NSR[23]",  (uint8_t *) vrs_desc, 0, &wrcallback);
	pos += dpos;

	memcpy(vrs_desc->identifier, VRS_TEA01, 5);
	udf_write_session_sector(udf_session, pos, "VRS TEA01",    (uint8_t *) vrs_desc, 0, &wrcallback);
	pos += dpos;
	/* followed by at least one blank block blanked up */

	free(vrs_desc);
}


/*
 * newfs_udf creates a new UDF filingsystem on a formatted disc. It is
 * restricted for now only create `simple' i.e. UDF specification discs.
 * 
 * For now, only CD-RW/DVD+RW filingsystems are implemented.
 *
 * FIXME we ought to determine UDF version numbers in advance so it can
 * easiliy be incorporated when nessisary.
 *
 * XXX allocation of space could be done smarter XXX
 */

void newfs_udf(struct udf_discinfo *disc, uint16_t dscr_ver, char *volset_name, char *privol_name, char *logvol_name, int vds_num, int max_vol_seq, uint32_t lb_mult, int udf_type) {
	struct long_ad           *fsd_loc;
	struct pri_vol_desc      *primary;
	struct anchor_vdp        *anchor;
	struct desc_tag          *terminator;
	struct part_desc         *partition;
	struct logvol_desc       *logvol;
	struct unalloc_sp_desc   *unallocsp;
	struct impvol_desc       *implvol;
	struct space_bitmap_desc *unalloc_space_bitmap_descr;
	struct fileset_desc      *fileset;
	struct udf_session       *udf_session;
	struct udf_pri_vol       *udf_pri_vol;
	struct udf_log_vol       *udf_log_vol;
	struct udf_partition     *udf_partition;
	struct udf_mountpoint    *udf_mountpoint;
	struct udf_node          *root_node, *dummy_node;
	struct udf_allocentry    *dscr_entry;
	struct udf_wrcallback     wrcallback;
	uint32_t bits;
	uint32_t bytes;
	uint32_t offset, pos, sector_size, lb_size;
	uint32_t anchor0, anchor1, anchor2, anchor2_rel;
	uint32_t lvd_length, lvd1_area, lvd2_area, end_lvd;
	uint32_t part_start, part_length, integrity_start, integrity_length;
	uint32_t unalloc_space_bitmap, space_bitmap_size;
	uint32_t start_lb_fsd, num_lbs_fsd;
	uint16_t serial, *udfver_pos, vpart_fsd;
	uint8_t  *blank;
	uint32_t cnt;
	int error;

	if (!disc->recordable) {
		fprintf(stderr, "Can't create filingsystem on a non recordable disc\n");
		return;
	}

	if (disc->sequential) {
		/* for sequential discs, close last track when nessisary */
		fprintf(stderr, "No support yet for creating filingsystem on sequential recordables\n");
		/* TODO recordable format */
		return;
	}
	if (!disc->sequential && !disc->rewritable) {
		/* non sequential WORM; not tested, no specimen */
		fprintf(stderr, "No support yet for non-sequential WORM devices\n");
		/* TODO non sequential WORM format */
		return;
	}

	/* TODO reuse parts of rewritable formatting for other types */

	if (disc->rewritable) {
		/* plain rewritable CD-RW or DVD+RW/DVD-RW, file etc. */
		switch (disc->disc_state) {
			case DISC_STATE_EMPTY:
				fprintf(stderr, "Disc is empty; please packet-format it before use\n");
				return;
			default:
			case DISC_STATE_INCOMPLETE :
				fprintf(stderr, "Disc is not properly formatted; its interrupted at formatting time\n");
				return;
			case DISC_STATE_FULL :
			case DISC_STATE_NOT_SERIAL :
				/* OK */
				break;
		}

		if (udf_discinfo_is_cd_or_dvd(disc) && disc->last_session_state != SESSION_STATE_COMPLETE) {
			fprintf(stderr, "Disc is marked being not serial, full, but the last session is not marked closed; Most likely formatting problem, try formatting it again\n");
			return;
		}

		if (disc->num_sessions > 1) {
			fprintf(stderr, "Can't handle multiple session rewritable discs yet\n");
			return;
		}

		/* rewritable format */
		printf("Creating a filingsystem on a recordable rewritable CD-RW or DVD+RW/DVD-RW or fixed length file\n");

		/* initialse statics */
		bzero(&wrcallback, sizeof(struct udf_wrcallback));

		STAILQ_INIT(&disc->sessions);

		wrcallback.function  = newfs_test_callback;		/* NULL for no callbacks */
/* XXX 		wrcallback.structure = (void *) 0xdeadbeef; */

		/* setup recording */
		disc->udf_recording = 1;
		udf_discinfo_set_recording_parameters(disc, 0);

		/* Set up disc space and create decscriptors */
		sector_size = disc->sector_size;
		lb_size     = sector_size * lb_mult;			/* express logical blocks as given multiple; normally 1 */

		blank = calloc(1, lb_size);
		assert(blank);

		/* note that session_end[0] is the first sector NOT adressable so substract one */
		serial      = 0;					/* primary starts at zero */
		offset      = 256;					/* first offset at sector number 256 to allow for prepending loader etc. (use 512 for recordables) */
		anchor0     = offset;
		anchor1     = disc->session_end[0] - 1;			/* only one session on rewritables */
		anchor2     = disc->session_end[0] - 256 - 1;		/* possible anchor, rather not use it on rewritables without sparables... */

		lvd_length  = MAX(UDF_READWRITE_LINE_LENGTH, disc->packet_size[0]);
		lvd1_area   = anchor0 + 1;
		lvd2_area   = lvd1_area + lvd_length;
		end_lvd     = lvd2_area + lvd_length;
	
		/* insert logical volume integrity sequence space if there is a logical volume */
		integrity_start  = 0;
		integrity_length = 0;
		if (logvol_name) {
			/*
			 * Maybe the space is a bit biggish but it means that
			 * 2 packet sized blocks can be scratched before the
			 * media needs to be reformatted. Minimum a line
			 * length to prevent multiple integrity descriptor
			 * writes to mess up other disc info.
			 */
			integrity_start  = end_lvd;

			integrity_length = MAX(UDF_READWRITE_LINE_LENGTH, 2*disc->packet_size[0]);
			assert(integrity_length * lb_size >= 8*1024);	/* UDF req. */

			end_lvd += integrity_length;
		}

		/* initial start of physical partion space */
		part_start  = end_lvd;

		/* partition size can be relative on anchor1 or anchor2; if on anchor1, anchor2 needs to be allocated */
		part_length = (sector_size * (uint64_t) (anchor2 - part_start)) / lb_size - 1;

		/* reserve space for unallocated space bitmap */
		bits  = part_length;
		bytes = (bits + 7)/8;
		space_bitmap_size = (bytes + sizeof(struct space_bitmap_desc)-1);

		/* round space bitmap size to sector size */
		/* FIXME: space bitmap recording on disc sector sizes or on lb_sizes? */
		space_bitmap_size = ((space_bitmap_size + lb_size - 1) / lb_size) * lb_size;

		/* sanity check to see if it can be formatted */
		if ((part_length <= 0) || (anchor1 <= 576)) {	/* XXX for now XXX */
			fprintf(stderr, "Too small a disc to be formatted with the UDF filingsystem\n");
			return;
		}

		/* build the anchors */
		error = udf_create_empty_anchor_volume_descriptor(sector_size, dscr_ver, lvd1_area, lvd2_area, lvd_length, &anchor);

		/* get udf_session structure */
		assert(disc->num_sessions == 1);
		udf_add_session_to_discinfo(disc, 0, anchor, 0);	/* inits complete session related structures */
		udf_session = STAILQ_FIRST(&disc->sessions);
		assert(udf_session);

		/* create primary and partition descriptor */
		unalloc_space_bitmap = 0;				/* allocate it at the start */

		error = udf_create_empty_primary_volume_descriptor(sector_size, dscr_ver, serial++, volset_name, privol_name, vds_num, max_vol_seq, &primary);
		error = udf_create_empty_partition_descriptor(sector_size, dscr_ver, serial++, 0, UDF_ACCESSTYPE_OVERWRITABLE, part_start, part_length, space_bitmap_size, unalloc_space_bitmap, &partition);

		/* process primary and partition */
		udf_proc_pri_vol(udf_session, &udf_pri_vol, primary);
		udf_proc_part(udf_pri_vol, &udf_partition, partition);

		/* all space in the partition is marked `free' at start */
		udf_mark_allocentry_queue(&udf_partition->unalloc_space_queue, lb_size, 0, (uint64_t) part_length * lb_size, UDF_SPACE_FREE, NULL, NULL);
		udf_partition->free_unalloc_space = (uint64_t) part_length * lb_size;

		/* create unallocated space descriptor and fill in its use in the partition space */
		/* (could be done a bit smarter) */
		error = udf_create_empty_space_bitmap(lb_size, dscr_ver, part_length, &unalloc_space_bitmap_descr);
		udf_partition->unalloc_space_bitmap = unalloc_space_bitmap_descr;

		udf_mark_allocentry_queue(&udf_partition->unalloc_space_queue, lb_size, (uint64_t) unalloc_space_bitmap * lb_size, space_bitmap_size, UDF_SPACE_ALLOCATED, NULL, NULL);
		udf_partition->free_unalloc_space -= space_bitmap_size;

		anchor2_rel = (anchor2*sector_size - part_start*sector_size) / lb_size;
		if (0) {
			if (anchor2_rel <= part_length) {
				/* overlap with anchor2 -> mark it in the unallocated space DESCRIPTOR, not in the unallocated space bitmap */
				udf_mark_allocentry_queue(&udf_partition->unalloc_space_queue, lb_size, (uint64_t) anchor2_rel * lb_size, lb_size, UDF_SPACE_ALLOCATED, NULL, NULL);
				udf_partition->free_unalloc_space -= lb_size;
			}
		}

		printf("Free unallocated space on this volume %"PRIu64"\n", udf_partition->free_unalloc_space);

		/* sync unallocated space descriptor (will be updated later?) */
		udf_sync_space_bitmap(&udf_partition->unalloc_space_queue, unalloc_space_bitmap_descr, lb_size);
		UDF_VERBOSE_MAX(udf_validate_tag_and_crc_sums((union dscrptr *) unalloc_space_bitmap_descr); udf_dump_descriptor((union dscrptr *) unalloc_space_bitmap_descr));

		/* proceed with the other descriptors */
		/* FIXME: space bitmap recording on disc sector sizes or on lb_sizes? */
		error = udf_create_empty_unallocated_space_descriptor(sector_size, dscr_ver, serial++, &unallocsp);
		if (logvol_name)  {
			/* wipe logical volume integrity descriptor sequence and `double check' ? */
			for (cnt = 0; cnt < integrity_length; cnt++) {
				udf_write_session_sector(udf_session, integrity_start + cnt, "blank", (uint8_t *) blank, 0, &wrcallback);
			}

			/* create logical volume */
			error = udf_create_empty_implementation_use_volume_descriptor(sector_size, dscr_ver, serial++, logvol_name, &implvol);
			assert(!error);
			error = udf_create_empty_logical_volume_descriptor(sector_size, dscr_ver, serial++, logvol_name, lb_size, integrity_start, integrity_length, &logvol);
			assert(!error);

			/* add partition mappings */
			switch (udf_type) {
				default :
				case DISC_TYPE_NORMAL   :
					/* add `normal' physical partition mapping */
					udf_add_physical_to_logvol(logvol, 1, 0);
					break;
				case DISC_TYPE_VIRTUAL  :
					/* XXX virtual on a cd-rw/dvd+rw? XXX */
					udf_add_physical_to_logvol(logvol, 0, 0);
					/* udf_add_virtual_to_logvol( logvol, 1, 0); */
					break;
				case DISC_TYPE_SPARABLE :
					/* udf_add_sparable_to_logvol(logvol, 0, 0); */
					break;
				case DISC_TYPE_META     :
					printf("No supported format type meta\n");
					break;
			}
			udf_log_vol = NULL;
			udf_proc_log_vol(udf_pri_vol, &udf_log_vol, logvol);
			udf_derive_new_logvol_integrity(udf_log_vol);
		}

		/* and finish the sequence */
		error = udf_create_empty_terminator_descriptor(sector_size, dscr_ver, &terminator);

		if (logvol_name) {
			/* allocate space for the filesets descriptor sequence */
			error = udf_allocate_lbs(udf_log_vol, UDF_C_DSCR, /* length */ 1, "Fileset sequence", &vpart_fsd, &start_lb_fsd, &num_lbs_fsd);

			DEBUG(printf("DEBUG: fsd op lbnum %d, vpart %d\n", start_lb_fsd, vpart_fsd));
			fsd_loc = &logvol->_lvd_use.fsd_loc;
			fsd_loc->len          = udf_rw32(num_lbs_fsd * lb_size);
			fsd_loc->loc.lb_num   = udf_rw32(start_lb_fsd);
			fsd_loc->loc.part_num = udf_rw16(vpart_fsd);

			/* create fileset(s) */
			error = udf_create_empty_fileset_desc(lb_size, dscr_ver, /*filesetnr*/ 0, logvol_name, "fileset", &fileset);
			assert(fileset);
			udf_proc_filesetdesc(udf_log_vol, fileset);

			/* create empty root-dir node */
			udf_mountpoint = SLIST_FIRST(&udf_mountables);
			udf_init_udf_node(udf_mountpoint, udf_log_vol, "Root", &root_node);
			udf_allocate_udf_node_on_disc(root_node);

			root_node->stat.st_size    = 0;
			root_node->stat.st_blksize = root_node->udf_log_vol->lb_size;
			root_node->stat.st_blocks  = 0;
			root_node->stat.st_mode    = 0777 | S_IFDIR;
			root_node->udf_filetype    = UDF_ICB_FILETYPE_DIRECTORY;
			root_node->unique_id       = 0;	/* UDF 2.3.6.5, 3.2.1.1. */

			root_node->udf_log_vol->num_directories++;
			udf_insert_node_in_hash(root_node);
			udf_node_mark_dirty(root_node);

			/* note creation times */
#ifndef NO_STAT_BIRTHTIME
			udf_set_timespec_now(&root_node->stat.st_birthtimespec);
#endif
			udf_set_timespec_now(&root_node->stat.st_atimespec);
			udf_set_timespec_now(&root_node->stat.st_ctimespec);
			udf_set_timespec_now(&root_node->stat.st_mtimespec);

			dscr_entry = TAILQ_FIRST(&root_node->dscr_allocs);
			fileset->rootdir_icb.loc.lb_num   = udf_rw32(dscr_entry->lb_num);
			fileset->rootdir_icb.loc.part_num = udf_rw16(dscr_entry->vpart_num);
			fileset->rootdir_icb.len          = udf_rw32(lb_size);			/* FIXME type 4096? */

			/* set all to writable or we're in trouble here */
			udf_log_vol->logvol_state = UDF_INTEGRITY_OPEN;
			udf_log_vol->writable     = 1;
			udf_mountpoint->writable  = 1;

			/* create `..' directory entry; hardlinks have no stat */
			error = udf_create_directory_entry(root_node, "..", UDF_ICB_FILETYPE_DIRECTORY, UDF_FILE_CHAR_DIR | UDF_FILE_CHAR_PAR, root_node, NULL, &dummy_node);
			assert(error == 0);

			/* adjust reference count for `root' since '..' points to it too but is not considered a link (ECMA 4/14.9.6, 4/8.8.3) */
			root_node->link_cnt--;
		}

		/* set all UDF version numbers to one and the same version */
		if (logvol_name) {
			/* Implementation use volume descritor's UDF version must be the same as the logical volume's UDF  version it describes */
			/* update/fill in the UDF version chosen */
			udfver_pos  = (uint16_t *) logvol->domain_id.id_suffix;
			*udfver_pos = udf_rw16(udf_log_vol->min_udf_writever);

			udfver_pos  = (uint16_t *) implvol->impl_id.id_suffix;
			*udfver_pos = udf_rw16(udf_log_vol->min_udf_writever);

			/* FIXME only one fileset now */
			udfver_pos  = (uint16_t *) fileset->domain_id.id_suffix;
			*udfver_pos = udf_rw16(udf_log_vol->min_udf_writever);
		}


		/* Start to WRITE OUT data VRS and UDF structures */
		writeout_vds(udf_session, dscr_ver);

		/* start writeout UDF structures */
#if 0
		/* wipe space around anchor2 */
		for (cnt=-20; cnt < 21; cnt++) {
			udf_write_session_sector(udf_session, anchor2 + cnt, "blank", (uint8_t *) blank, 0, &wrcallback);
		}
#endif
			udf_write_session_descriptor(udf_session, anchor0, "Anchor",          (union dscrptr*) anchor,     &wrcallback);
			udf_write_session_descriptor(udf_session, anchor1, "Anchor",          (union dscrptr*) anchor,     &wrcallback);
			udf_write_session_descriptor(udf_session, anchor2, "Anchor",          (union dscrptr*) anchor,     &wrcallback);

		/* writeout volume space */
		pos = lvd1_area;
			udf_write_session_descriptor(udf_session, pos++,   "Primary",         (union dscrptr*) primary,    &wrcallback);
			udf_write_session_descriptor(udf_session, pos++,   "Partiton",        (union dscrptr*) partition,  &wrcallback);
			udf_write_session_descriptor(udf_session, pos++,   "Unalloc space",   (union dscrptr*) unallocsp,  &wrcallback);
		if (logvol_name) {
			udf_write_session_descriptor(udf_session, pos++,   "Logvol",          (union dscrptr*) logvol,     &wrcallback);
			udf_write_session_descriptor(udf_session, pos++,   "Impl. volume",    (union dscrptr*) implvol,    &wrcallback);
		}
			udf_write_session_descriptor(udf_session, pos++,   "Terminator",      (union dscrptr*) terminator, &wrcallback);

		pos = lvd2_area;
			udf_write_session_descriptor(udf_session, pos++,   "Primary",         (union dscrptr*) primary,    &wrcallback);
			udf_write_session_descriptor(udf_session, pos++,   "Partiton",        (union dscrptr*) partition,  &wrcallback);
			udf_write_session_descriptor(udf_session, pos++,   "Unalloc space",   (union dscrptr*) unallocsp,  &wrcallback);
		if (logvol_name) {
			udf_write_session_descriptor(udf_session, pos++,   "Logvol",          (union dscrptr*) logvol,     &wrcallback);
			udf_write_session_descriptor(udf_session, pos++,   "Impl. volume",    (union dscrptr*) implvol,    &wrcallback);
		}
			udf_write_session_descriptor(udf_session, pos++,   "Terminator",      (union dscrptr*) terminator, &wrcallback);

		/* the unallocated space bitmap gets written out on sync/dismount */
		if (logvol_name) {
			udf_write_logvol_descriptor(udf_log_vol, vpart_fsd, start_lb_fsd, "File set", (union dscrptr*) fileset, &wrcallback);
		}

		return;
	}

	/*
	 * If we reach here, we obviously missed a class of recording devices,
	 * better give a error and abort
	 */

	fprintf(stderr, "Internal error: unknown recording class of devices encountered; aborting\n");
	return;
}



int usage(char *program) {
	fprintf(stderr, "Usage %s [options] devicename\n", program);
	fprintf(stderr, "Creates a filingsystem on file or a formatted disc\n");
	fprintf(stderr, "-S volsetname     use `volsetname as volume set name'\n"
			"-P primaryname    use `primaryname' as primary volume name\n"
			"-L volumename     use `volumename' as logical volume name (discname)\n"
			"-v volumenumber   when part of a set use this volumenumber\n"
			"-m volumenumber   maximum volumenumber in this set\n"
			"-2                alter descriptor version number (default 3)\n"
			"-s numsect        create image with number of sectors (file only)\n"
			"-b blocksize      use alternative sectorsize; use only on files/discs\n"
			"-B mult           multiplier for logical sectors (NON-standard!!)\n"
			"-u level          UDF system verbose level\n"
			"-D                debug/verbose SCSI command errors\n"
	);
	fprintf(stderr, "use `dd if=/dev/zero bs=64k of=discimage.cd count=...` to create a new discfile. `count` must be >= 19 (about 1.2Mb)\n");
	fprintf(stderr, "or use the `-b' and `-s' flags to create a new discfile. blocksize needs to be a multiple of 512\n");
	return 1;
}


int main(int argc, char *argv[]) {
	struct timeval time_of_day;
	struct udf_discinfo *disc;
	char *progname, *volset_name, *privol_name, *logvol_name;
	uint64_t volset_nr;
	uint32_t vds_num, max_vol_seq, dscr_ver;
	uint32_t alt_sector_size, alt_num_sect, lb_mult;
	off_t fsize;
	int   flag, error, fhandle;

	progname = argv[0];
	if (argc == 1) return usage(progname);

	volset_name = NULL;
	privol_name = NULL;
	logvol_name = NULL;
	vds_num     = 1;
	max_vol_seq = 1;
	dscr_ver    = 3;
	alt_sector_size = 0;
	alt_num_sect    = 0;
	lb_mult         = 1;

	while ((flag = getopt(argc, argv, "S:P:L:v:m:2s:b:B:u:D")) != -1) {
		switch (flag) {
			case 'S' :
				volset_name = strdup(optarg);
				break;
			case 'P' :
				privol_name = strdup(optarg);
				break;
			case 'L' :
				logvol_name = strdup(optarg);
				break;
			case 'v' :
				vds_num = atoi(optarg);
				break;
			case 'm' :
				max_vol_seq = atoi(optarg);
				break;
			case '2' :
				dscr_ver = 2;
				break;
			case 's' :
				alt_num_sect = atoi(optarg);
				break;
			case 'b' :
				alt_sector_size = atoi(optarg);
				break;
			case 'B' :
				printf("-B option to set logvol multiplier temporarely disabled\n");
				/* lb_mult = atoi(optarg); */
				break;
			case 'u' :
				udf_verbose = atoi(optarg);
				break;
			case 'D' :
				uscsilib_verbose = 1;
				break;
			default  :
				return usage(progname);
		}
	}
	argv += optind;
	argc -= optind;

	if (argc < 1) return usage(progname);

	srandom(time(NULL));
	if (!volset_name) {
		volset_name = malloc(128);
		(void)gettimeofday(&time_of_day, NULL);
		volset_nr  =  (uint64_t) random();
		volset_nr |= ((uint64_t) time_of_day.tv_sec) << 32;
		sprintf(volset_name, "%0"PRIx64, volset_nr);
	}
	if (!privol_name) {
		privol_name = malloc(32);
		sprintf(privol_name, "%08lx", random());
	}
	if (!logvol_name) {
		fprintf(stderr, "newfs_udf: no logical volume name passed; not creating logical volume descriptor\nYOU PROLLY DONT WANT THIS\n");
	}
	if ((vds_num < 1) || (vds_num > max_vol_seq)) {
		fprintf(stderr, "Invalid volume seqence number or out of bounds\n");
		return 1;
	}
	if ((dscr_ver < 2) || (dscr_ver > 3)) {
		fprintf(stderr," UDF upto version 2.50 only supports descriptor versions 2 and 3\n");
		return 1;
	}
	
	/* just one device allowed */
	SLIST_INIT(&udf_discs_list);
	printf("Opening device %s\n\n", *argv);

	error = udf_open_disc(*argv, /* discop_flags */ 0, &disc);
	if (error) {
		error = 0;
		if ((alt_num_sect > 0) && (alt_sector_size > 0)) {
			/* create a file */
			fprintf(stderr, "Creating new disc image\n");
			fsize = (off_t) alt_num_sect * alt_sector_size;
			fhandle = open(*argv, O_CREAT | O_TRUNC | O_RDWR, 0660);
			if (fhandle) {
				fsize = ftruncate(fhandle, fsize);
				if (fsize < 0)
					error = errno;
				close(fhandle);
			} else {
				error = errno;
			}
		}
		if (!error) {
			error = udf_open_disc(*argv, /* discop_flags */ 0, &disc);
		}
		if (error) {
			fprintf(stderr, "Can't open my device; bailing out : %s\n", strerror(error));
			exit(1);
		}
	}
	SLIST_INSERT_HEAD(&udf_discs_list, disc, next_disc);	/* better add it to the disc list */

	/* try to set the alternative sector size */
	if (alt_sector_size || alt_num_sect) {
		error = udf_discinfo_alter_perception(disc, alt_sector_size, alt_num_sect);
		if (error) {
			exit(0);
		}
	}

	udf_unix_init();
	udf_start_unix_thread();

	printf("\n\n");
	udf_dump_discinfo(disc);

	/* now do the real thing */
	newfs_udf(disc, dscr_ver, volset_name, privol_name, logvol_name, vds_num, max_vol_seq, lb_mult, DISC_TYPE_NORMAL);
	
	printf("Closing disc\n");

	udf_dismount_disc(disc);

	return 0;
}

