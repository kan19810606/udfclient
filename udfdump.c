/* $NetBSD$ */

/*
 * File "udfdump.c" is part of the UDFclient toolkit.
 * File $Id: udfdump.c,v 1.75 2016/04/25 21:01:41 reinoud Exp $ $Name:  $
 *
 * Copyright (c) 2003, 2004, 2005, 2006, 2011
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
#include "udf.h"
#include "udf_bswap.h"


/* switches */

/* #define DEBUG(a) (a) */
#define DEBUG(a) if (0) { a; }
int dump_sequential	 = 0;
int dump_directory_trees = 0;

#ifndef MAX
#	define MAX(a,b) ((a)>(b)?(a):(b))
#	define MIN(a,b) ((a)<(b)?(a):(b))
#endif

/* include the dump parts ... in order to get a more sane splitting */
extern void udf_dump_descriptor(union dscrptr *dscrpt);
extern void udf_dump_alive_sets(void);
extern void udf_dump_root_dir(struct udf_mountpoint *mountpoint);


/* globals */
extern int udf_verbose;
extern int uscsilib_verbose;


/* main discdump routine */

void udf_dump_sequential(int alt_sector_size, int flags, char *dev_name) {
	struct udf_discinfo *disc;
	union dscrptr	*dscr;
	uint64_t	 sec_num, rsec_num;
	uint32_t	 sector_size, size, chunk, dscr_len;
	uint8_t		*new_sector, *sector, *pos;
	int		 error;

	udf_verbose     = UDF_VERBLEV_ACTIONS;

	printf("Opening disc:\n");
	error = udf_open_disc(dev_name, flags, &disc);
	if (error) return;

	if (alt_sector_size)
		disc->sector_size = alt_sector_size;

	sector = calloc(1, 65*1024);
	assert(sector);

	printf("Sequential dump of device/file (sector size %d)\n", disc->sector_size);
	sec_num = 0;
	sector_size = disc->sector_size;
	do {
		size  = 1;
		error = udf_read_physical_sectors(disc, sec_num, size, "sector", sector);

		if (!error) {
			dscr = (union dscrptr *) sector;
			if (!udf_check_tag(dscr)) {
				if (udf_rw16(dscr->tag.id) <= TAGID_MAX) {
					dscr_len = udf_calc_tag_malloc_size(dscr, sector_size);
					dscr_len = MAX(sector_size, dscr_len);
					if (dscr_len > sector_size) {
						size = (dscr_len + sector_size -1) / sector_size;
						new_sector = realloc(sector, size * sector_size);
						if (!new_sector)
							goto next_sector;
						if (new_sector)
							sector = new_sector;

						dscr   = (union dscrptr *) sector;
						chunk  = MIN(size, 0xffff);
						pos = sector;
						rsec_num = sec_num;
						while (size) {
							error = udf_read_physical_sectors(
									disc, rsec_num, chunk, "sector", pos);
							if (error)
								break;
							pos      += chunk * sector_size; 
							rsec_num += chunk;
							size     -= chunk;
							chunk     = MIN(size, 0xffff);
						}
						if (error)
							break;
					}
					size = (dscr_len + sector_size -1) / sector_size;

					if (!udf_check_tag_payload(dscr)) {
						printf("%8d ", (int) sec_num); udf_dump_descriptor(dscr);
					}
				}
			}
		} else {
			if (error == ENOENT)
				break;
			printf("\nRead error : %s\n", strerror(error));
		}
next_sector:
		sec_num++;
		printf("\r%08d ", (int) sec_num);
		fflush(stdout);
	} while (1);
	printf("\n");
}


void udf_dump_mountable_dirtrees(void) {
	struct udf_mountpoint *mountpoint;

	printf("Dumping directory tree for each non obsolete logical volume's fileset\n");
	SLIST_FOREACH(mountpoint, &udf_mountables, all_next) {
		printf("\n");
		printf("Directory tree of mount point %s\n", mountpoint->mount_name);
		udf_dump_root_dir(mountpoint);
	}
}


int usage(char *program) {
	fprintf(stderr, "Usage %s [options] devicename [devicename]*)\n", program);
	fprintf(stderr, "-S		dump device sequential\n"
			"-s		byteswap read sectors (for PVRs)\n"
			"-t		dump complete directory contents\n"
			"-b blocksize	use alternative sectorsize; use only on files/discs\n"
			"-u level	UDF system verbose level\n"
			"-D		verbose SCSI command errors\n"
			"-r range	only use UDF sessions range like -3,4-5 or 6-\n"
	);
	return 1;
}


int main(int argc, char *argv[]) {
	struct udf_discinfo *disc;
	char *progname, *range;
	uint32_t alt_sector_size;
	int flag, mntflags, bswap, error;

	progname = argv[0];
	if (argc == 1) return usage(progname);

	dump_sequential      = 0;
	dump_directory_trees = 0;
	range                = NULL;
	alt_sector_size      = 0;
	bswap                = 0;
	while ((flag = getopt(argc, argv, "u:r:b:DSst")) != -1) {
		switch (flag) {
			case 'u' :
				udf_verbose = atoi(optarg);
				break;
			case 'r' :
				range = strdup(optarg);
				if (udf_check_session_range(range)) {
					fprintf(stderr, "Invalid range %s\n", range);
					return usage(progname);
				}
				break;
			case 'b' :
				alt_sector_size = atoi(optarg);
				break;
			case 'D' :
				uscsilib_verbose = 1;
				break;
			case 'S' :
				dump_sequential = 1;
				break;
			case 's' :
				bswap = 1;
				break;
			case 't' :
				dump_directory_trees = 1;
				break;
			default  :
				return usage(progname);
		}
	}
	argv += optind;
	argc -= optind;

	if (dump_sequential) {
		if (argc != 1) return usage(progname);
		udf_dump_sequential(alt_sector_size, bswap, *argv);
		return 0;
	}

	if (argc == 0) return usage(progname);

	/* all other arguments are devices */
	udf_init();
	while (argc) {
		printf("Opening device %s\n\n", *argv);
		mntflags  = UDF_MNT_RDONLY;
		mntflags |= bswap ? UDF_MNT_BSWAP : 0;
		error = udf_mount_disc(*argv, range, alt_sector_size, mntflags, &disc);
		if (error) {
			fprintf(stderr, "Can't open my device; bailing out : %s\n", strerror(error));
			exit(1);
		}

		argv++; argc--;
		if (udf_verbose) printf("\n\n");
	}

	printf("Resulting list of alive sets :\n\n");
	udf_dump_alive_sets();

	if (dump_directory_trees) {
		printf("Dump tree of all mountables\n");
		udf_dump_mountable_dirtrees();
		printf("\n");
	}

	printf("Closing discs\n");
	SLIST_FOREACH(disc, &udf_discs_list, next_disc) {
		udf_close_disc(disc);
	}

	return 0;
}

