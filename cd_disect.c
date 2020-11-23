/* $NetBSD$ */

/*
 * File "cd_disect.c" is part of the UDFclient toolkit.
 * File $Id: cd_disect.c,v 1.81 2017/04/03 08:48:51 reinoud Exp $ $Name:  $
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
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <strings.h>
#include <assert.h>
#include <ctype.h>
#include <sys/types.h>
#include <inttypes.h>

#include "uscsilib.h"


#ifndef MAX
#	define MAX(a,b) ((a)>(b)?(a):(b))
#	define MIN(a,b) ((a)<(b)?(a):(b))
#endif


/*
 * from inquiry preriph. device type; used to saveguard MMC specific
 * operations. See spc2.
 */

#define DEVICE_TYPE_MMC 0x05


/* globals */
struct uscsi_dev dev;


/* helper functions */
static int read_cd_hex2(int val) {
	int nl, nh;

	nl = val & 15;
	nh = val >> 4;
	if (nl >= 'A') nl -= 'A' + 10;
	if (nh >= 'A') nh -= 'A' + 10;

	return (nh*16) + nl;
}


static int read_cd_bcd(int val) {
	int nl, nh;

	nl = (val & 15) - '0';
	nh = (val >> 4) - '0';
	if ((nl < 0 || nl > 9) || (nh < 0 || nh > 9)) return val;

	return nh*10 + nl;
}


static int32_t cd_msf2lba(int h, int m, int s, int f) {
	return 270000*h + 4500*m + 75*s + f - 150;
}



/* start of discect functions */
static char *print_disc_type(int type) {
	switch (type) {
		case 0x00 : return "CD-DA or CD-ROM Disc or non CD";
		case 0x10 : return "CD-I Disc";
		case 0x20 : return "CD-ROM XA Disc";
		case 0xFF : return "Undefined";
	}
	return "Reserved";
}


static char *print_disc_state(int state) {
	switch (state) {
		case 0 : return "empty disc";
		case 1 : return "incomplete (appendable)";
		case 2 : return "full (not appendable)";
		case 3 : return "random writable";
	}
	return "unknown disc state";
}


static char *print_session_state(int state) {
	switch (state) {
		case 0 : return "empty";
		case 1 : return "incomplete";
		case 2 : return "reserved/damaged";
		case 3 : return "complete/closed disc";
	}
	return "unknown session_state";
}


static char *print_mmc_profile(int profile) {
	static char scrap[100];

	switch (profile) {
		case 0x00 : return "Unknown[0] profile";
		case 0x01 : return "Non removeble disc";
		case 0x02 : return "Removable disc";
		case 0x03 : return "Magneto Optical with sector erase";
		case 0x04 : return "Magneto Optical write once";
		case 0x05 : return "Advance Storage Magneto Optical";
		case 0x08 : return "CD-ROM";
		case 0x09 : return "CD-R recordable";
		case 0x0a : return "CD-RW rewritable";
		case 0x10 : return "DVD-ROM";
		case 0x11 : return "DVD-R sequential";
		case 0x12 : return "DVD-RAM rewritable";
		case 0x13 : return "DVD-RW restricted overwrite";
		case 0x14 : return "DVD-RW sequential";
		case 0x15 : return "DVD-R dual layer sequential";
		case 0x16 : return "DVD-R dual layer jump";
		case 0x17 : return "DVD-RW dual layer";
		case 0x18 : return "DVD-Download disc";
		case 0x1a : return "DVD+RW rewritable";
		case 0x1b : return "DVD+R recordable";
		case 0x20 : return "DDCD readonly (retracted)";
		case 0x21 : return "DDCD-R recordable (retracted)";
		case 0x22 : return "DDCD-RW rewritable (retracted)";
		case 0x2a : return "DVD+RW double layer";
		case 0x2b : return "DVD+R double layer";
		case 0x40 : return "BD-ROM";
		case 0x41 : return "BD-R Sequential Recording (SRM)";
		case 0x42 : return "BD-R Random Recording (RRM)";
		case 0x43 : return "BD-RE rewritable";
		case 0x50 : return "HD DVD-ROM (retracted)";
		case 0x51 : return "HD DVD-R (retracted)";
		case 0x52 : return "HD DVD-RAM (retracted)";
		case 0x53 : return "HD DVD-RW (retracted)";
		case 0x58 : return "HD DVD-R dual layer (retracted)";
		case 0x5a : return "HD DVD-RW dual layer (retracted)";
	}
	sprintf(scrap, "Reserved profile 0x%02x", profile);
	return scrap;
}


static char *print_write_type(int type) {
	switch (type) {
		case 0x00 : return "Packet/Incremental";
		case 0x01 : return "Track-at-once";
		case 0x02 : return "Session-at-one";
		case 0x03 : return "Raw";
	}
	return "unknown write type";
}


static char *print_data_block_type(int type) {
	static char scrap[100];

	switch (type) {
		case  0 : return "raw data 2352 bytes";
		case  1 : return "raw data 2368 bytes with P and Q channel";
		case  2 : return "raw data 2352 bytes (+96) P-W subchannel appended";
		case  3 : return "raw data 2352 bytes (+96) raw P-W subchannel appended";
		case  8 : return "ISO mode 1 with 2048 bytes data";
		case  9 : return "ISO mode 2 with 2336 bytes data, formless";
		case 10 : return "ISO mode 2 with 2048 bytes data (CDROM-XA, form 1), subheader from write parameters";
		case 11 : return "ISO mode 2 with 2048 bytes data (CDROM-XA, form 1), 8 bytes for subheader first";
		case 12 : return "ISO mode 2 with 2324 bytes data (CDROM-XA, form 2), subheader from write parameters";
		case 13 : return "ISO mode 2 with 2332 bytes data (CDROM-XA, form 1 or 2 or mixed), 8 bytes for subheader first";
	}
	sprintf(scrap, "Unknown/reserved data block type 0x%02x", type);
	return scrap;
}


static char *print_Q_control(int cntrl) {
	static char scrap[100];

	strcpy(scrap, "");
	if ((cntrl & 12) == 4) {
		strcat(scrap, "data track ");
		if (cntrl & 1) strcat(scrap, "; incremental  "); else strcat(scrap, "; uninterrupted");
	} else {
		strcat(scrap, "audio track");
		if (cntrl & 1) strcat(scrap, "; pre-emphasis of 50/15 microseconds"); else strcat(scrap, "; no pre-emphasis");
	}
	if (cntrl & 2) strcat(scrap, "; copy prohibited");

	return scrap;
}


static char *print_session_format(int format) {
	static char scrap[100];

	switch (format) {
		case 0x00 : return "CD-DA, CD-ROM or other data discs";
		case 0x10 : return "CD-I disc";
		case 0x20 : return "CD-ROM XA disc or DDCD disc";
	}
	sprintf(scrap, "Unknown/reserved session format type 0x%02x", format);
	return scrap;
}


/* why disc_type is not equal to session_format is not clear yet; its reported in the TOC/PMA/ATI format 010b */
static char *print_TOC_disc_type(int type) {
	static char scrap[100];

	switch (type) {
		case 0x00 : return "CD-DA or CD Data disc with first track in Mode 1";
		case 0x10 : return "CD-I disc";
		case 0x20 : return "CD data XA disc with first track in Mode 2";
	}
	sprintf(scrap, "Unknown/reserved TOC disc type type 0x%02x", type);
	return scrap;
}


static char *print_inactivity_time(int time) {
	static char scrap[100];

	switch (time) {
		case 0x0 : return "Vendor specific";
		case 0x1 : return "125 ms";
		case 0x2 : return "250 ms";
		case 0x3 : return "500 ms";
		case 0x4 : return "1 sec";
		case 0x5 : return "2 sec";
		case 0x6 : return "4 sec";
		case 0x7 : return "8 sec";
		case 0x8 : return "16 sec";
		case 0x9 : return "32 sec";
		case 0xa : return "1 min";
		case 0xb : return "2 min";
		case 0xc : return "4 min";
		case 0xd : return "8 min";
		case 0xe : return "16 min";
		case 0xf : return "32 min";
	}
	sprintf(scrap, "Unknown/reserved inactivity timeout 0x%02x", time);
	return scrap;
}


static char *printdevice_type(int type) {
	static char scrap[100];

	switch (type) {
		case 0x00 : return "Direct-access device (e.g., magnetic disk)";
		case 0x01 : return "Sequential-access device (e.g., magnetic tape)";
		case 0x02 : return "Printer device";
		case 0x03 : return "Processor device";
		case 0x04 : return "Write-once device (e.g., some optical disks)";
		case 0x05 : return "CD-ROM device";
		case 0x06 : return "Scanner";
		case 0x07 : return "Optical memory device (e.g., some optical disks)";
		case 0x08 : return "Medium changer device (e.g., jukeboxes)";
		case 0x09 : return "Communications device";
		case 0x0a : /* fall trough */
		case 0x0b : return "Defined by ASC IT8 (Graphic arts pre-press devices)";
		case 0x0c : return "Storage array controller device (e.g., RAID)";
		case 0x0d : return "Enclosure services device";
		case 0x0e : return "Simplified direct-access device (e.g., magnetic disk)";
		case 0x0f : return "Optical card reader/writer device";
		case 0x10 : return "Reserved/used for Bridging Expanders";
		case 0x11 : return "Object-based Storage Device";
	}
	sprintf(scrap, "Unknown/reserved device type 0x%02x", type);
	return scrap;
}


static char *printdevice_qualifier(int device_qualifier) {
	static char scrap[100];

	switch (device_qualifier) {
		case 0x00 : return "Device server is capable and device is connected";
		case 0x01 : return "Device server is capable but device is not connected";
		case 0x02 : return "Reserved";
		case 0x03 : return "Device server is not capable of supporting this device";
	}
	sprintf(scrap, "Unknown/reserved device qualifier 0x%02x", device_qualifier);
	return scrap;
}


static char *printstandards_version(int version) {
	static char scrap[100];

	switch (version) {
		case 0x00 : return "Does not claim conformance to any standard";
		case 0x01 : return "SCSI (obsolete)";
		case 0x03 : return "The device complies to ANSI INCITS 301-1997 (SPC)";
		case 0x04 : return "The device complies to ANSI INCITS 351-2001 (SPC-2)";
		case 0x05 : return "The device complies to ANSI INCITS T10/1416-D (SPC-3)";
	}
	sprintf(scrap, "Unknown/Obsolete/reserved (0x%02x)", version);
	return scrap;
}


static char *print_normal_string(uint8_t *buf, int len) {
	static char scrap[100];
	char *pos;
	int i;

	memset(scrap, 0, 100);
	pos = scrap;
	for (i = 0; i < len; i++) {
		if (isprint(buf[i]))
			*pos++ = buf[i];
	}
	*pos = (char) 0;
	return scrap;
}


void dump_drive_identify(int *device_type) {
	scsicmd cmd;
	uint8_t buf[100];
	int device_qual, rmb, version, additional_len, total_len;
	int i, error;

	/* go for SCB for a start */
	*device_type = 0;

	bzero(cmd, SCSI_CMD_LEN);
	cmd[0] = 0x12;	/* INQUIRY */
	cmd[1] = 0;	/* basic inquiry */
	cmd[2] = 0;	/* no page or operation code */
	cmd[3] = 0;	/* reserved/MSB result */
	cmd[4] = 96;	/* all but vendor specific */
	cmd[5] = 0;	/* control */
	error = uscsi_command(SCSI_READCMD, &dev, cmd, 6, buf, 96, 30000, NULL);
	if (error) {
		fprintf(stderr, "Device Inquiry returned error : %s\n", strerror(error));
		return;
	}

	*device_type   = buf[0] & 0x1f;
	device_qual    = buf[0] >> 16;
	rmb            = buf[1] & 0x80;
	version        = buf[2];
	additional_len = buf[4];
	total_len      = additional_len + 4;

	printf("\n");
	printf("\tDevice qualfier\t\t: %s\n", printdevice_qualifier(device_qual));
	printf("\tDevice type\t\t: %s\n", printdevice_type(*device_type));
	printf("\tMedia type\t\t: %s\n", rmb? "Removable": "Fixed");
	printf("\tConforming to standard\t: %s\n", printstandards_version(version));
	printf("\tVendor identification\t: %s\n",  print_normal_string(buf +  8,  8));
	printf("\tProduct identification\t: %s\n", print_normal_string(buf + 16, 16));
	printf("\tProduct revision level\t: %s\n", print_normal_string(buf + 32,  4));
	if (total_len < 36)
		goto out;
	printf("\tVendor specific\t\t: %s\n",        print_normal_string(buf + 36, 19));
	if (total_len < 58)
		goto out;

	printf("\tComplies to:\n");
	for (i = 58; i <= 72; i+=2) {
		if (i >= total_len)
			break;
		version = buf[i+1] | (buf[i] << 8);
		printf("\t\t0x%04x\n", version);
	}
out:
	printf("\t<Rest not dumped yet>\n");
	printf("\n");
}


void dump_recorded_capacity(void) {
	scsicmd cmd;
	uint8_t buf[36];
	uint32_t lba, blk_len;
	int error;

	bzero(cmd, SCSI_CMD_LEN);
	cmd[0] = 0x25;	/* CD READ RECORDED CAPACITY */
	error = uscsi_command(SCSI_READCMD, &dev, cmd, 10, buf, 8, 30000, NULL);
	if (error) {
		fprintf(stderr, "Read recorded capacity SCSI call returned : %s\n", strerror(error));
		return;
	}

	lba     = buf[3] | (buf[2]<<8) | (buf[1]<<16) | (buf[0]<<24);
	blk_len = buf[7] | (buf[6]<<8) | (buf[5]<<16) | (buf[4]<<24);
	printf("\nCD recorded capacity is LBA %d (%d blocks), blk_len %d\n",
			lba, lba/4, blk_len);
}


void dump_feature(uint8_t *fpos) {
	uint32_t	 feature, cnt, profile;
	uint32_t	 feature_len, feature_ver, feature_cur, feature_pers;
	uint32_t	 interface, blocking, log_blk_size, last_log_blk_addr;
	uint32_t	 datablock_types, num_link_sizes, cuesheet_len;
	uint32_t	 num_vol_levels, dcb_entry;
	uint32_t	 century, year, day, month, hour, minute, second;
	uint8_t		*pos;

	feature     =  fpos[1] | (fpos[0] << 8);
	feature_ver = (fpos[2] >> 2) & 15;
	feature_cur = (fpos[2] & 1);
	feature_pers= (fpos[2] & 2);
	feature_len =  fpos[3];
	printf("\t\tFeature  0x%04x (%2d bytes) version %2d; persistent %s; currently active %s\n",
			feature, feature_len, feature_ver, feature_pers?"yes":" no", feature_cur?"yes":" no");

	pos = &fpos[4];
	switch (feature) {
		case 0x0000:
			printf("\t\tProfile list; supporting profiles\n");
			for (cnt=0; cnt < feature_len; cnt += 4) {
				profile = pos[1] | (pos[0] << 8);
				printf("\t\t\t%s %s\n", pos[2] & 1 ?"ACTIVE  ":"inactive", print_mmc_profile(profile));
				pos += 4;
			}
			break;
		case 0x0001:
			interface = pos[3] | (pos[2] << 8) | (pos[1] << 16) | (pos[0] << 24);
			printf("\t\tCore features : physical interface standard commands for `");
			switch (interface) {
				case 0      : printf("Unspecified"); break;
				case 1      : printf("SCSI family"); break;
				case 2      : printf("ATAPI"); break;
				case 3      : printf("IEEE 1394 - 1995"); break;
				case 4      : printf("IEEE 1394A"); break;
				case 5      : printf("Fibre channel"); break;
				case 6      : printf("IEEE 1394B"); break;
				case 7      : printf("Serial ATAPI"); break;
				case 8      : printf("USB (1.1 or 2.0)"); break;
				case 0xffff : printf("Vendor specific"); break;
				default     : printf("<reserved (0x%04x)>", interface); break;
			}
			printf("'\n");
			break;
		case 0x0002:
			printf("\t\tMorphing command set; %ssupport for ASYNC\n", (pos[0] & 1)?"":"no ");
			break;
		case 0x0003:
			printf("\t\tRemovable medium features\n");
			if (feature_ver > 1) {
				printf("\t\t\tUnknown flags\n");
				break;
			}
			if (pos[0] & 1) printf("\t\t\tDevice can be locked against removal\n");
			printf("\t\t\tDevice will go into the %slocked state by default\n", (pos[0] & 4)?"":"un-");
			printf("\t\t\tDevice %s eject the media with START/STOP with eject bit\n", (pos[0] & 8)?"can":"can't");
			printf("\t\t\tLoading mechanism : ");
			switch (pos[0] >> 5) {
				case 0  : printf("Caddy");  break;
				case 1  : printf("Tray");   break;
				case 2  : printf("Pop-up"); break;
				case 4  : printf("Embedded changer with separate discs"); break;
				case 5  : printf("Embedded changes using magazines"); break;
				default : printf("Unknown/Reserved");
			}
			printf("\n");
			break;
		case 0x0004:
			printf("\t\tWrite protect features :");
			if (pos[0] & 2) printf(" SPWP");
			if (pos[0] & 1) printf(" SSWPP");
			printf("\n");
			break;
		case 0x0010:
			printf("\t\tRandom readable\n");
			log_blk_size = pos[3] | (pos[2] << 8) | (pos[1] << 16) | (pos[0] << 24);
			blocking     = pos[5] | (pos[4] << 8);
			printf("\t\t\tLogical block size %u bytes\n", log_blk_size);
			printf("\t\t\tDevice blocking number %d logical blocks\n", blocking);
			if (pos[6] & 1) printf("\t\t\tHas RW error recovery mode page\n");
			break;
		case 0x001d:
			printf("\t\tMulti-read; The Logical Unit can read all CD media types; based on OSTA MultiRead\n");
			break;
		case 0x001e:
			printf("\t\tAbility to read CD specific structures\n");
			printf("\t\t\tDevice does %ssupport C2 error pointers\n", (pos[0] & 2)?"":"NOT ");
			printf("\t\t\tDevice does %ssupport CD-Text\n", (pos[0] & 1)?"":"NOT ");
			break;
		case 0x001f:
			printf("\t\tAbility to read DVD specific structures\n");
			break;
		case 0x0020:
			printf("\t\tRandom writable\n");
			last_log_blk_addr = pos[3] | (pos[2] << 8) | (pos[1] << 16) | (pos[0] << 24);
			log_blk_size      = pos[7] | (pos[6] << 8) | (pos[5] << 16) | (pos[4] << 24);
			blocking          = pos[9] | (pos[8] << 8);
			printf("\t\t\tLast writable logic block address is %u ", last_log_blk_addr);
			printf("(approx %u Mb)\n", (uint32_t) ((uint64_t) last_log_blk_addr*log_blk_size/(1024*1024)));
			printf("\t\t\tLogical block size %u bytes\n", log_blk_size);
			printf("\t\t\tDevice blocking number %d logical blocks\n", blocking);
			if (pos[10] & 1) printf("\t\t\tHas RW error recovery mode page\n");
			break;
		case 0x0021:
			printf("\t\tIncremental streaming writable\n");
			datablock_types = pos[1] | (pos[0] << 8);
			num_link_sizes = pos[3];
			printf("\t\t\tDevice is %scapable of `Zero loss linking'\n", (pos[2] & 1)?"":"NOT ");
			printf("\t\t\tDevice supported data types (bitfield) 0x%04x\n", datablock_types);
			printf("\t\t\tDevice supports %d link size%s :", num_link_sizes, (num_link_sizes != 1)?"s":"");
			for (cnt=0; cnt < num_link_sizes; cnt++) {
				printf(" %d", pos[4+cnt]);
			}
			printf("\n");
			break;
		case 0x0022:
			printf("\t\tCan erase/support for erasing media (OBSOLETE)\n");
			break;
		case 0x0023:
			printf("\t\tSupport for formatting media\n");
			break;
		case 0x0024:
			printf("\t\tHas defect handling; i.e. apparently defect-free space\n");
			printf("\t\t\tDevice does %ssupport read `Space Area Information' (DVD)\n", (pos[0] & 128)?"":"NOT ");
			break;
		case 0x0025:
			printf("\t\tSupport for writing any unrecorded logical block on write once media in random order\n");
			log_blk_size = pos[3] | (pos[2] << 8) | (pos[1] << 16) | (pos[0] << 24);
			blocking     = pos[5] | (pos[4] << 8);
			printf("\t\t\tLogical block size %u bytes\n", log_blk_size);
			printf("\t\t\tDevice blocking number %d logical blocks\n", blocking);
			if (pos[6] & 1) printf("\t\t\tHas RW error recovery mode page\n");
			break;
		case 0x0026:
			printf("\t\tSupport for restricted overwrite; i.e. on blocking boundaries only\n");
			break;
		case 0x0027:
			printf("\t\tCD-RW CAV Write; The ability to write high speed CD-RW media\n");
			/* parameters all reserved */
			break;
		case 0x0028:
			printf("\t\tMRW formatted media support\n");
			printf("\t\t\tDevice can read MRW formatted media\n");
			printf("\t\t\tDevice %swrite/format media in MRW format\n", (pos[0] & 1)?"can ":"can't");
			break;
		case 0x002a:
			printf("\t\tDVD+RW media reading/writing support\n");
			if (pos[0] & 1) {
				printf("\t\t\tDevice can write and background format DVD+RW media\n");
				if (pos[1] & 1) printf("\t\t\tDevice only supports read compatibility format stop\n");
			} else {
				printf("\t\t\tDevice can only recognise and read DVD+RW\n");
			}
			break;
		case 0x002b:
			printf("\t\tThe ability to read/write DVD+R recorded media\n");
			if (pos[0] & 1) {
				printf("\t\t\tDevice can write DVD+R media\n");
			} else {
				printf("\t\t\tDevice can only recognise and read DVD+R\n");
			}
			break;
		case 0x002c:
			printf("\t\tSupport for rigid restricted overwrite only (CD-RW)\n");
			if (pos[0] & 8) printf("\t\t\tDevice can generate direct status data during formatting\n");
			if (pos[0] & 4) printf("\t\t\tDevice can read the defect status data recorded on media\n");
			if (pos[0] & 2) printf("\t\t\tDevice allows writing on immediate state sessions and quick formatting\n");
			printf("\t\t\tDevice does %ssupport blanking of media\n", (pos[0] & 1)?"":"NOT ");
			break;
		case 0x002d:
			printf("\t\tTrack at once recording support\n");
			datablock_types = pos[3] | (pos[2] << 8);
			if (pos[0] & 64) printf("\t\t\tDevice is capable of zero-loss linking\n");
			if (pos[0] & 16) printf("\t\t\tDevice is capable of writing R-W Sub code in RAW mode\n");
			if (pos[0] &  8) printf("\t\t\tDevice is capable of writing R-W Sub code in Packet mode\n");
			printf("\t\t\tDevice does %ssupport test writes (i.e. laser off)\n", (pos[0] & 4)?"": "NOT ");
			if (pos[0] &  2) printf("\t\t\tDevice supports overwriting a track using track at once\n");
			if (pos[0] &  1) printf("\t\t\tDevice supports writing R-W Sub channels with user data\n");
			printf("\t\t\tTrack at once data types supported (bitfield) 0x%04x\n", datablock_types);
			break;
		case 0x002e:
			printf("\t\tSession at once or RAW writing support; CD Mastering\n");
			cuesheet_len = pos[3] | (pos[2] << 8) | (pos[1] << 16);
			if (pos[0] & 64) printf("\t\t\tDevice is capable of zero-loss linking\n");
			printf("\t\t\tDevice can %swrite using the Session at Once write type\n", (pos[0] & 32)?"":"CANT ");
			if (pos[0] & 16) printf("\t\t\tDevice is capable of writing multi-session in RAW mode\n");
			printf("\t\t\tDevice can %swrite using the raw write type\n", (pos[0] & 8)?"":"CANT ");
			printf("\t\t\tDevice does %ssupport test writes (i.e. laser off)\n", (pos[0] & 4)?"": "NOT ");
			if (pos[0] &  2) printf("\t\t\tDevice supports overwriting previous recorded media\n");
			if (pos[0] &  1) printf("\t\t\tDevice supports writing R-W Sub channels with user data\n");
			printf("\t\t\tMaximum cue sheet length %d bytes\n", cuesheet_len);
			break;
		case 0x002f:
			printf("\t\tDVD-R/-RW Write; The ability to write DVD specific structures\n");
			if (pos[0] & 64) printf("\t\t\tDevice is capable of zero-loss linking\n");
			printf("\t\t\tDevice does %ssupport test writes (i.e. laser off)\n", (pos[0] & 4)?"": "NOT ");
			if (pos[0] &  2) printf("\t\t\tDevice supports overwriting previous recorded media\n");
			break;
		case 0x0030:
			printf("\t\tCan read DDCD user data\n");
			break;
		case 0x0031:
			printf("\t\tCan write and read DDCD-R media\n");
			printf("\t\t\tDevice does %ssupport test writes (i.e. laser off)\n", (pos[0] & 4)?"": "NOT ");
			break;
		case 0x0032:
			printf("\t\tCan write and read DDCD-RW media\n");
			if (pos[0] & 2) printf("\t\t\tDevice allows writing on immediate state sessions and quick formatting\n");
			printf("\t\t\tDevice does %ssupport blanking of media\n", (pos[0] & 1)?"":"NOT ");
			break;
		case 0x0033:
			printf("\t\tLayer jump recording feature\n");
			num_link_sizes = pos[3];
			printf("\t\t\tDevice supports %d link size%s :", num_link_sizes, (num_link_sizes != 1)?"s":"");
			for (cnt=0; cnt < num_link_sizes; cnt++) {
				printf(" %d", pos[4+cnt]);
			}
			printf("\n");
			break;
		case 0x0037:
			printf("\t\tCD-RW Media Write Support\n");
			printf("\t\t\tCan %swrite media subtype 0\n", (pos[1] &   1) ? "": "NOT ");
			printf("\t\t\tCan %swrite media subtype 1\n", (pos[1] &   2) ? "": "NOT ");
			printf("\t\t\tCan %swrite media subtype 2\n", (pos[1] &   4) ? "": "NOT ");
			printf("\t\t\tCan %swrite media subtype 3\n", (pos[1] &   8) ? "": "NOT ");
			printf("\t\t\tCan %swrite media subtype 4\n", (pos[1] &  16) ? "": "NOT ");
			printf("\t\t\tCan %swrite media subtype 5\n", (pos[1] &  32) ? "": "NOT ");
			printf("\t\t\tCan %swrite media subtype 6\n", (pos[1] &  64) ? "": "NOT ");
			printf("\t\t\tCan %swrite media subtype 7\n", (pos[1] & 128) ? "": "NOT ");
			break;
		case 0x0038:
			printf("\t\tBD-R Pseudo-Overwrite (POW) support feature\n");
			break;
		case 0x003b:
			printf("\t\tDVD+R Double Layer support\n");
			printf("\t\t\tDrive can read DVD+R double layer discs\n");
			printf("\t\t\tDrive can %swrite DVD+R double layer discs (only valid when active)\n", (pos[1] & 1) ? "" : "NOT ");
			break;
		case 0x0040:
			printf("\t\tBluRay disc formats read support :\n");
			printf("\t\t\tAble to read control structures and user data from the BD disc\n");
			printf("\t\t\tDrive does %ssupport reporting disc BCA data\n", (pos[0] & 1)?"":"NOT ");
			printf("\t\t\tCan %sread BD-ROM media\n", (pos[21] &   2) ? "": "NOT ");
			printf("\t\t\tCan %sread BD-R media\n", (pos[13] &   2) ? "": "NOT ");
			printf("\t\t\tCan %sread BD-RE media version 1\n", (pos[5] & 2) ? "": "NOT ");
			printf("\t\t\tCan %sread BD-RE media version 2\n", (pos[5] & 4) ? "": "NOT ");
			if (feature_ver == 0)
				printf("\t\t\tOther obsolete BD version read flags not dumped\n");
			break;
		case 0x0041:
			printf("\t\tBluRay disc formats write support :\n");
			printf("\t\t\twrite command does %ssupport Verify-Not-Required (VNR) bit\n", (pos[0] & 1)?"":"NOT ");
			printf("\t\t\tCan %swrite BD-R media\n", (pos[13] &   2) ? "": "NOT ");
			printf("\t\t\tCan %swrite BD-RE media version 1\n", (pos[5] & 2) ? "": "NOT ");
			printf("\t\t\tCan %swrite BD-RE media version 2\n", (pos[5] & 4) ? "": "NOT ");
			break;
		case 0x0100:
			printf("\t\tPower management support\n");
			break;
		case 0x0101:
			printf("\t\tS.M.A.R.T. (Self-Monitoring Analysis and Reporting Technology support)\n");
			printf("\t\t\tDevice does %sprovide Fault/Failure Reporting Mode Page\n", (pos[0] & 1)?"":"NOT ");
			break;
		case 0x0102:
			printf("\t\tEmbedded changer feature\n");
			printf("\t\t\tDevice is %scapable of switching media sides\n", (pos[0] & 16)?"":"NOT ");
			printf("\t\t\tDevice can %sreport disc presence after a reset of magazine change\n", (pos[0] & 4)?"":"NOT ");
			break;
		case 0x0103:
			printf("\t\tAbility to play audio CDs via the Logical Unit's own analog output\n");
			num_vol_levels = pos[3] | (pos[2] << 8);
			printf("\t\t\tDevice does %ssupport the SCAN command\n", (pos[0] & 4)?"":"NOT ");
			printf("\t\t\tDevice %s mute separate channels\n", (pos[0] & 4)?"can":"can't");
			printf("\t\t\tDevice %s set volumes for each channel separately\n", (pos[0] & 4)?"can":"can't");
			printf("\t\t\tDevice has %d audio volume levels\n", num_vol_levels);
			break;
		case 0x0104:
			printf("\t\tAbility for the device to accept new microcode via the interface\n");
			break;
		case 0x0105:
			printf("\t\tAbility to respond to all commands within a specific time\n");
			break;
		case 0x0106:
			printf("\t\tAbility to perform DVD CSS/CPPM authentication and RPC\n");
			printf("\t\t\tSupporting CCS version %d\n", pos[3]);
			break;
		case 0x0107:
			printf("\t\tAbility to read and write using Initiator requested performance parameters; realtime streaming\n");
			printf("\t\t\tDevice does %shave the `read buffer capacity' command \n", (pos[0] & 16)?"":"NOT ");
			printf("\t\t\tDevice CD speed can %sbe set up\n", (pos[0] & 8)?"":"NOT ");
			printf("\t\t\tDevice write speed performance can %sbe queried\n", (pos[0] & 4)?"":"NOT ");
			printf("\t\t\tDevice does %shave a `set streaming' command\n", (pos[0] & 2)?"":"NOT ");
			printf("\t\t\tDevice does %ssupport stream recording operation\n", (pos[0] & 1)?"":"NOT ");
			break;
		case 0x0108:
			printf("\t\tThis device has an unique serialnumber : \"");
			for (cnt=0; cnt < feature_len; cnt++) {
				printf("%c", pos[cnt]);
			}
			printf("\"\n");
			break;
		case 0x010a:
			printf("\t\tThe ability to read and/or write Disc Control Blocks (DVD only)\n");
			for (cnt=0; cnt < feature_len; cnt+=4, pos+=4) {
				dcb_entry = pos[3] | (pos[2] << 8) | (pos[1] << 16);
				printf("\t\t\t\tSupported Disc Control Blocks content descriptor %d\n", dcb_entry);
			}
			printf("\n");
			break;
		case 0x010b:
			printf("\t\tThe Logical Unit supports DVD CPRM authentication\n");
			printf("\t\t\tSupporting CPRM version %d\n", pos[3]);
			break;
		case 0x010c:
			printf("\t\tFirmware Information feature :\n");
			century = (pos[ 1] - '0') + 10*(pos[ 0] - '0');
			year    = (pos[ 3] - '0') + 10*(pos[ 2] - '0');
			month   = (pos[ 5] - '0') + 10*(pos[ 4] - '0');
			day     = (pos[ 7] - '0') + 10*(pos[ 6] - '0');
			hour    = (pos[ 9] - '0') + 10*(pos[ 8] - '0');
			minute  = (pos[11] - '0') + 10*(pos[10] - '0');
			second  = (pos[13] - '0') + 10*(pos[12] - '0');
			printf("\t\t\tFirmware version date %d-%d-%2d%02d at time %02d:%02d:%02d\n", day, month, century, year, hour, minute, second);
			break;
		case 0x010d:
			printf("\t\tAACS feature :\n");
			printf("\t\t\tDrive does %ssupports reading the drive certificate (RDC)\n",              (pos[0] & 16)?"":"NOT ");
			printf("\t\t\tDrive does %ssupports reading the media key block (RMC)\n",                (pos[0] &  8)?"":"NOT ");
			printf("\t\t\tDrive does %ssupport writing sectors with bus encription (WBE)\n",         (pos[0] &  4)?"":"NOT ");
			printf("\t\t\tDrive does %ssupport bus encryption (BEC)\n",                              (pos[0] &  2)?"":"NOT ");
			printf("\t\t\tDrive does %ssupport generating the Binding-Nonce (%d blocks required)\n", (pos[0] &  1)?"":"NOT ", pos[1]);
			printf("\t\t\tDrive supports maximal %d AGID's concurrently\n", pos[2] & 15);
			printf("\t\t\tAACS version supported v %d\n", pos[3]);
			break;
		case 0x01ff:
			printf("\t\tFirmware creation date report :\n");
			century = pos[ 1] | (pos[ 0] << 8);
			year	= pos[ 3] | (pos[ 2] << 8);
			month	= pos[ 5] | (pos[ 4] << 8);
			day	= pos[ 7] | (pos[ 6] << 8);
			hour	= pos[ 9] | (pos[ 8] << 8);
			minute	= pos[11] | (pos[10] << 8);
			printf("\t\t\tFirmware version date %d-%d-%2d%2d at time %02d:%02d\n", day, month, century, year, hour, minute);
			break;
		default:
			break;
	}
	printf("\n");
}


#define max_feat_tbl_len 64*1024	/* don't change */
void dump_drive_configuration(uint32_t feature_current) {
	scsicmd		cmd;
	uint8_t		features[max_feat_tbl_len], *fpos;
	uint32_t	feature, last_feature, features_len, feat_tbl_len;
	uint32_t	current_profile, pos;
	uint32_t	feature_cur, feature_len;
	int		error;

	printf("Getting drive configuration\n");

	/* get total length */
	last_feature = feature = 0;
	feat_tbl_len = 8;

	bzero(cmd, SCSI_CMD_LEN);
	cmd[0] = 0x46;			/* Get configuration */
	cmd[1] = 0;			/* RT=0 -> all independent of current setting */
	cmd[2] = (last_feature) >> 8;	/* MSB feature number */
	cmd[3] = (last_feature) & 0xff;	/* LSB feature number */
	cmd[7] = (feat_tbl_len) >> 8;	/* MSB buffersize */
	cmd[8] = (feat_tbl_len) & 0xff;	/* LSB buffersize */
	cmd[9] = 0;			/* control */
	error = uscsi_command(SCSI_READCMD, &dev, cmd, 10, features, feat_tbl_len, 10000, NULL);
	if (error) {
		fprintf(stderr, "While reading feature table length : %s\n", strerror(error));
		return;
	}
	features_len    = features[3] | (features[2]<<8) | (features[1]<<16) | (features[0]<<24);
	current_profile = features[7] | (features[6]<<8);


	printf("\tTotal features table length %d bytes\n", features_len);
	printf("\tCurrent profile       0x%04x `%s'\n", current_profile, print_mmc_profile(current_profile));

	/* getting feature table size */
	last_feature = feature = 0;
	do {
		feat_tbl_len = 8;

		bzero(cmd, SCSI_CMD_LEN);
		cmd[0] = 0x46;			/* Get configuration */
		cmd[1] = 0;			/* RT=0 -> all independent of current setting */
		cmd[2] = (last_feature) >> 8;	/* MSB feature number */
		cmd[3] = (last_feature) & 0xff;	/* LSB feature number */
		cmd[7] = (feat_tbl_len) >> 8;	/* MSB buffersize */
		cmd[8] = (feat_tbl_len) & 0xff;	/* LSB buffersize */
		cmd[9] = 0;			/* control */
		error = uscsi_command(SCSI_READCMD, &dev, cmd, 10, features, feat_tbl_len, 10000, NULL);
		if (error) {
			fprintf(stderr, "While reading feature table length : %s\n", strerror(error));
			return;
		}

		/* actually request them */
		feat_tbl_len = features[3] | (features[2]<<8) | (features[1]<<16) | (features[0]<<24);
		feat_tbl_len = MIN(max_feat_tbl_len, feat_tbl_len);

		bzero(cmd, SCSI_CMD_LEN);
		cmd[0] = 0x46;			/* Get configuration */
		cmd[1] = 0;			/* RT=0 -> all independent of current setting */
		cmd[2] = (last_feature) >> 8;	/* MSB feature number */
		cmd[3] = (last_feature) & 0xff;	/* LSB feature number */
		cmd[7] = (feat_tbl_len) >> 8;	/* MSB buffersize */
		cmd[8] = (feat_tbl_len) & 0xff;	/* LSB buffersize */
		cmd[9] = 0;			/* control */
		error = uscsi_command(SCSI_READCMD, &dev, cmd, 10, features, feat_tbl_len, 10000, NULL);
		if (error) {
			fprintf(stderr, "While reading feature table : %s\n", strerror(error));
			return;
		}

		pos = 8;
		while (pos < feat_tbl_len) {
			fpos = &features[pos];

			feature     =  fpos[1] | (fpos[0] << 8);
			feature_cur = (fpos[2] & 1);
			feature_len =  fpos[3];
			if (feature_cur == feature_current) dump_feature(fpos);

			last_feature = MAX(last_feature, feature);
			if ((feature_len & 3) != 0) {
				printf("\t\t*** drive returned bad feature length ***\n");
				dump_feature(fpos);
				feature_len = (feature_len + 3) & ~3;
			}
			pos += 4 + feature_len;
		}
		if (feat_tbl_len >= 0xffff)
			printf("WARNING: requesting 2nd chunk, not tested\n");
	} while (feat_tbl_len >= 0xffff);
}
#undef max_feat_tbl_len


#define max_di_len 1000
#define max_ti_len  128
void dump_disc_information(void) {
	scsicmd		 cmd;
	uint8_t		 di[max_di_len], ti[max_ti_len];
	int		 di_len = max_di_len, ti_len = max_ti_len;
	int		 disc_status, disc_type, last_session_state, eraseable;
	int		 num_sessions;
	int		 first_track, first_track_last_session, last_track_last_session;
	int		 data_length, is_track_number, is_session_number, is_track_mode, is_data_mode;
	int		 is_copy, is_damage, is_fixed_packet, is_packet_or_inc, is_blank, is_reserved;
	int		 nwa_valid, lra_valid;
	uint32_t	 track_start, next_writable_addr, free_blocks, packet_size, track_size, last_recorded_addr;
	uint32_t	 disc_id, disc_barcode_l, disc_barcode_h;
	uint64_t	 disc_barcode;
	int		 track, speed, pos, num_opc_tables;
	int		 printed, error;

	bzero(di, di_len);
	bzero(ti, ti_len);

	/* read in fixed part */
	di_len = 34;

	bzero(cmd, SCSI_CMD_LEN);
	cmd[0] = 0x51;				/* Read disc information */
	cmd[7] = 0;				/* MSB allocation length */
	cmd[8] = di_len;			/* LSB allocation length */
	cmd[9] = 0;				/* control */
	error = uscsi_command(SCSI_READCMD, &dev, cmd, 10, di, di_len, 10000, NULL);
	if (error) {
		fprintf(stderr, "While reading disc information : %s\n", strerror(error));
		return;
	}

	di_len = di[1] | (di[0]<<8);
	bzero(cmd, SCSI_CMD_LEN);
	cmd[0] = 0x51;				/* Read disc information */
	cmd[7] = di_len >> 8;			/* MSB allocation length */
	cmd[8] = di_len & 0xff;			/* LSB allocation length */
	cmd[9] = 0;				/* control */
	error = uscsi_command(SCSI_READCMD, &dev, cmd, 10, di, di_len, 10000, NULL);
	if (error) {
		fprintf(stderr, "While reading disc information : %s\n", strerror(error));
		return;
	}

	di_len = di[1] | (di[0]<<8);
	printf("\tDrive returned %d bytes of data\n", di_len);

	disc_type                =  di[ 8];
	disc_status		 =  di[ 2]       &  3;
	last_session_state	 = (di[ 2] >> 2) &  3;
	eraseable		 =  di[ 2]       & 16;
	first_track		 =  di[ 3];
	num_sessions		 =  di[ 4] | (di[ 9] << 8);
	first_track_last_session =  di[ 5] | (di[10] << 8);
	last_track_last_session  =  di[ 6] | (di[11] << 8);
	disc_id                  =  di[15] | (di[14]<<8) | (di[13]<<16) | (di[12]<<24);
	disc_barcode_l		 =  di[31] | (di[30]<<8) | (di[29]<<16) | (di[28]<<24);
	disc_barcode_h		 =  di[27] | (di[26]<<8) | (di[25]<<16) | (di[24]<<24);
	disc_barcode		 =  ((uint64_t) disc_barcode_h << 32) | disc_barcode_l;
	num_opc_tables           =  di[33];

	if (di_len < 33) num_opc_tables = 0;

	printf("\tDisc type             : %s\n", print_disc_type(disc_type));
	printf("\tDisc status           : %s\n", print_disc_state(disc_status));
	printf("\tKind of disc          : %s\n", eraseable ? "eraseable (rewritable disc)": "NOT eraseable (recordable disc)");
	printf("\tFirst track number    : %d\n", first_track);
	printf("\tNumber of sessions    : %d\n", num_sessions);
	printf("\tDrive does %ssupport setting OPC information; num tables %d\n", num_opc_tables ? "": "NOT ", num_opc_tables);
	printf("\tLast session information :\n");
	printf("\t\tState : %s\n", print_session_state(last_session_state));
	printf("\t\tFrom track %d to track %d (including hidden)\n", first_track_last_session, last_track_last_session);
	printf("\tDisc is %sin 'Restricted Use Mode'\n", (di[7] & 32) ? "": "NOT ");
	switch (di[7] & 3) {
		case 0 : printf("\tBackground formatting not applicable"); break;
		case 1 : printf("\tDisc formatting was interrupted and not running now"); break;
		case 2 : printf("\tDisc is being formatted in the background"); break;
		case 3 : printf("\tDisc is formatted"); break;
	}
	printf("\n");
	printf("\tDisc has %s valid 'Disc ID'               : (0x%08"PRIx32")\n",  (di[7] & 128) ? "a ": "NO", disc_id);
	printf("\tDisc has %s valid 'Disc Bar Code'         : (0x%016"PRIx64")\n", (di[7] &  64) ? "a ": "NO", disc_barcode);
	printf("\tDisc has %s valid 'Disc Application code' : (0x%x)\n",           (di[7] &  16) ? "a" : "NO", di[32]);
	printf("\tLast Session  Lead-in  Start Address not dumped\n");
	printf("\tLast Possible Lead-out Start Address not dumped\n");

	printf("\n");
	if (di_len < 33) {
		printf("\tDrive reported no recording speeds and OPC data\n");
	} else {
		printf("\tRecording speeds (in kB/sec) (OPC data not dumped) :\n\t\t");
		pos = 34;
		printed = 0;
		while (pos < di_len) {
			speed = di[pos+1] | (di[pos] << 8);
			if (speed) {
				printed = 1;
				printf("%d", speed);
				if (pos < di_len-8) printf(", ");
			}
			pos += 8;
		}
		if (!printed)
			printf("\t\tAparently no speeds were returned");
		printf("\n");
	}

	printf("\nReading track and session information; track by track\n");
	for (track = first_track; track <= last_track_last_session; track++) {
		ti_len = 39 + 1;			/* make ATAPI happy       */

		bzero(cmd, SCSI_CMD_LEN);
		bzero(ti, ti_len);
		cmd[0] = 0x52;				/* Read track information */
		cmd[1] = 1;				/* indexed on track       */
		cmd[4] = track >> 8;			/* track number 0-0xff ?  */
		cmd[5] = track & 0xff;
		cmd[7] = ti_len >> 8;
		cmd[8] = ti_len & 0xff;
		cmd[9] = 0;
		error = uscsi_command(SCSI_READCMD, &dev, cmd, 10, ti, ti_len, 10000, NULL);
		if (error) {
			fprintf(stderr, "While reading track info : %s\n", strerror(error));
			break;
		}
		data_length		= ti[1] | (ti[0]  << 8);
		is_track_number		= ti[2] | (ti[32] << 8);
		is_session_number	= ti[3] | (ti[33] << 8);
		is_track_mode		= ti[5] & 15;
		is_copy			= ti[5] & 16;
		is_damage		= ti[5] & 32;
		is_data_mode		= ti[6] & 15;
		is_fixed_packet		= ti[6] & 16;
		is_packet_or_inc	= ti[6] & 32;
		is_blank		= ti[6] & 64;
		is_reserved		= ti[6] & 128;
		is_data_mode		= ti[6] & 15;
		nwa_valid		= ti[7] & 1;
		lra_valid		= ti[7] & 2;
		track_start		= ti[11] | (ti[10]<<8) | (ti[ 9]<<16) | (ti[ 8]<<24);
		next_writable_addr	= ti[15] | (ti[14]<<8) | (ti[13]<<16) | (ti[12]<<24);
		free_blocks		= ti[19] | (ti[18]<<8) | (ti[17]<<16) | (ti[16]<<24);
		packet_size		= ti[23] | (ti[22]<<8) | (ti[21]<<16) | (ti[20]<<24);
		track_size		= ti[27] | (ti[26]<<8) | (ti[25]<<16) | (ti[24]<<24);
		last_recorded_addr	= ti[31] | (ti[30]<<8) | (ti[29]<<16) | (ti[28]<<24);

		if (data_length <= 30) {
			/* 8 bits track and session numbers returned; last_recordable is invalid */
			is_track_number   &= 0xff;
			is_session_number &= 0xff;
			last_recorded_addr = 0;
		}
		printf("\tTrack %d of session %d\n", is_track_number, is_session_number);
		if (data_length < 27 && lra_valid) printf("\t\tLast recorded addres valid but not returned\n");
		printf("\t\tStart at       : %d\n", track_start);
		printf("\t\tLength         : %d\n", track_size);
		printf("\t\tTrackmode      : %s\n", print_Q_control(is_track_mode));
		printf("\t\tDatamode       : %s\n", (is_data_mode == 1) ? "mode 1" : "mode 2");
		if (free_blocks)      printf("\t\tFree blocks    : %d\n", free_blocks);
		if (packet_size)      printf("\t\tPacket size    : %d\n", packet_size);
		if (nwa_valid)        printf("\t\tNext writable  : %d\n", next_writable_addr);
		if (lra_valid)        printf("\t\tLast recorded  : %d\n", last_recorded_addr);
		if (is_copy)	      printf("\t\tTrack is a copy\n");
		if (is_damage)	      printf("\t\tTrack is damaged\n");
		if (is_reserved)      printf("\t\tReserved or Complete track\n");
		if (is_blank)	      printf("\t\tBlank track\n");
		if (is_packet_or_inc) printf("\t\tPacket mode track\n");
		if (is_fixed_packet)  printf("\t\tFixed packet sizes track\n");

		if (data_length > ti_len) {
			printf("\t\tRest %d bytes undumed\n", data_length - ti_len);
		}
	}
}
#undef di_len
#undef ti_len


void dump_format_capabilities(void) {
	scsicmd		cmd;
	uint8_t		buf[512], *fcd;
	uint32_t	num_blks, param;
	char           *format_str, *nblks_str, *param_str;
	int		dscr_type, list_length, format_type;
	int		buf_len = 512, trans_len;
	int		error;

	bzero(buf, buf_len);

	trans_len = 12;				/* only fixed header first */
	bzero(cmd, SCSI_CMD_LEN);
	cmd[0] = 0x23;				/* Read format capabilities */
	cmd[7] = trans_len >> 8;		/* MSB allocation length */
	cmd[8] = trans_len & 0xff;		/* LSB allocation length */
	cmd[9] = 0;				/* control */
	error = uscsi_command(SCSI_READCMD, &dev, cmd, 10, buf, trans_len, 10000, NULL);
	if (error) {
		fprintf(stderr, "While reading format capabilities : %s\n", strerror(error));
		return;
	}

	list_length = buf[ 3];

	printf("\tCurrent/max capacity followed by additional capacity, reported length of %d bytes (8/entry)\n", list_length);
	if (list_length % 8) {
		printf("\t\tWarning: violating SCSI spec, capacity list length ought to be multiple of 8\n");
		printf("\t\tInterpreting as including header of 4 bytes\n");
		assert(list_length % 8 == 4);
		list_length -= 4;
	}

	/* read in full capacity list */
	trans_len = 12 + list_length;		/* complete structure */
	bzero(cmd, SCSI_CMD_LEN);
	cmd[0] = 0x23;				/* Read format capabilities */
	cmd[7] = trans_len >> 8;		/* MSB allocation length */
	cmd[8] = trans_len & 0xff;		/* LSB allocation length */
	cmd[9] = 0;				/* control */
	error = uscsi_command(SCSI_READCMD, &dev, cmd, 10, buf, trans_len, 10000, NULL);
	if (error) {
		fprintf(stderr, "While reading format capabilities : %s\n", strerror(error));
		return;
	}

	fcd = buf + 4;
	list_length -= 4;			/* seems to include the first entry */
	while (list_length > 0) {
		num_blks    = fcd[ 3] | (fcd[ 2] << 8) |  (fcd[ 1] << 16) | (fcd[ 0] << 24);
		dscr_type   = fcd[ 4] & 3;
		format_type = fcd[ 4] >> 2;
		param       = fcd[ 7] | (fcd[ 6] << 8) |  (fcd[ 5] << 16);

		format_str = nblks_str = param_str = "reserved";
		switch (format_type) {
			case  0x00 :
				format_str = "full format capacity (non packet)";
				nblks_str  = "sectors";
				param_str  = "block length in bytes";
				break;
			case  0x01 :
				format_str = "spare area expansion";
				nblks_str  = "extension in blocks";
				param_str  = "block length in bytes";
				break;
			/* 0x02 - 0x03 reserved */
			case  0x04 :
				format_str = "variable length zone'd format";
				nblks_str  = "zone length";
				param_str  = "zone number";
				break;
			case  0x05 :
				format_str = "fixed length zone'd format";
				nblks_str  = "zone length";
				param_str  = "last zone number";
				break;
			/* 0x06 - 0x0f reserved */
			case  0x10 :
				format_str = "CD-RW/DVD-RW full packet format";
				nblks_str  = "adressable blocks";
				param_str  = "fixed packet size/ECC blocksize in sectors";
				break;
			case  0x11 :
				format_str = "CD-RW/DVD-RW grow session";
				nblks_str  = "adressable blocks";
				param_str  = "fixed packet size/ECC blocksize in sectors";
				break;
			case  0x12 :
				format_str = "CD-RW/DVD-RW add session";
				nblks_str  = "adressable blocks";
				param_str  = "maximum fixed packet size/ECC blocksize in sectors";
				break;
			case  0x13 :
				format_str = "DVD-RW max growth of last complete session";
				nblks_str  = "adressable blocks";
				param_str  = "ECC blocksize in sectors";
				break;
			case  0x14 :
				format_str = "DVD-RW max quick grow last session";
				nblks_str  = "adressable blocks";
				param_str  = "ECC blocksize in sectors";
				break;
			case  0x15 :
				format_str = "DVD-RW quick full format";
				nblks_str  = "adressable blocks";
				param_str  = "ECC blocksize in sectors";
				break;
			/* 0x16 - 0x23 reserved */
			case  0x24 :
				format_str = "MRW format";
				nblks_str  = "Defect Management Area blocks";
				param_str  = "not used";
				break;
			/* 0x25 reserved */
			case  0x26 :
				format_str = "DVD+RW full format";
				nblks_str  = "sectors";
				param_str  = "not used";
				break;
			/* 0x27 - 0x2f reserved */
			case  0x30 :
				format_str = "BD-RE full format with spare area";
				nblks_str  = "blocks";
				param_str  = "total spare area size in clusters";
				break;
			case  0x31 :
				format_str = "BD-RE full format without spare area";
				nblks_str  = "blocks";
				param_str  = "block length in bytes";
				break;
			/* 0x32 - 0x3f reserved */
			default :
				break;
		}
		printf("\n\tFormat type 0x%02x : %s\n", format_type, format_str);

		switch (dscr_type) {
			case  1 : printf("\t\tUnformatted media, maximum formatted capacity\n"); break;
			case  2 : printf("\t\tFormatted media, current formatted capacity\n");   break;
			case  3 : printf("\t\tNo media present or incomplete session, maximum formatted capacity\n"); break;
			default : printf("\t\tUnspecified descriptor type\n"); break;
		}
		printf("\t\tNumber of blocks : %12d\t(%s)\n", num_blks, nblks_str);
		printf("\t\tParameter        : %12d\t(%s)\n", param, param_str);

		fcd += 8;
		list_length-=8;
	}
	printf("\n\tNote: last entry might be all zero due to deviation from SCSI standard\n");
}


void dump_formatted_toc(void) {
	scsicmd		 cmd;
	uint8_t		 toc[10000];
	uint32_t	 lba;
	int		 pos, toc_len, data_length;
	int		 cur_track, addr, tno, cntrl;
	int		 first_track, last_track;
	int		 min, sec, frame;
	int		 error;

	printf("\nFormatted TOC\n");

	/* only fixed part first */
	toc_len = 4;

	/* just TOC */
	bzero(cmd, SCSI_CMD_LEN);
	cmd[0] = 0x43;			/* READ TOC/PMA/ATIP INFO	*/
	cmd[1] = 2;			/* return HMSF			*/
	cmd[2] = 0;			/* format 0; formatted TOC	*/
	cmd[6] = 1;			/* start at first track 	*/
	cmd[7] = toc_len >> 8;
	cmd[8] = toc_len & 0xff;
	cmd[9] = 0;			/* control			*/
	error = uscsi_command(SCSI_READCMD, &dev, cmd, 10, toc, toc_len, 10000, NULL);
	if (error) {
		fprintf(stderr, "TOC reading of tracks failed : %s\n", strerror(error));
		return;
	}

	first_track = read_cd_hex2(toc[2]);
	last_track  = read_cd_hex2(toc[3]);

	printf("\tFirst track   : %d\n", first_track);
	printf("\tLast track    : %d\n", last_track);
	printf("\n");

	/* complete formatted TOC */
	data_length =toc[1] | (toc[0] << 8);
	toc_len = data_length + 2;

	cmd[7] = toc_len >> 8;
	cmd[8] = toc_len & 0xff;
	error = uscsi_command(SCSI_READCMD, &dev, cmd, 10, toc, toc_len, 10000, NULL);
	if (error) {
		fprintf(stderr, "TOC reading of complete formatted TOC failed : %s\n", strerror(error));
		return;
	}

	/* dump formatted TOC */
	data_length =toc[1] | (toc[0] << 8);
	pos = 4; cur_track = 0;
	while (pos < data_length + 2) {
		cntrl = (toc[pos + 1]     ) & 15;
		addr  = (toc[pos + 1] >> 4) & 15;
		tno   = read_cd_bcd(toc[pos + 2]);
		/* hour  = read_cd_bsd(toc[pos + 4]); symetry, though mandatory zero ? */
		min   = read_cd_bcd(toc[pos + 5]);
		sec   = read_cd_bcd(toc[pos + 6]);
		frame = read_cd_bcd(toc[pos + 7]);

		lba   = cd_msf2lba(0, min,  sec,  frame);

		if (tno != cur_track) {
			if (tno == 0xAA) {
				printf("\tLead-out area start");
			} else {
				printf("\tTrack %d start\t", tno);
			}
		}
		cur_track = tno;

		printf("\t%02d:%02d:%02d (LBA %8d)\t", min, sec, frame, lba);
		printf(" (%s), ", print_Q_control(cntrl));

		printf("Q sub-channel ");
		switch (addr) {
			case 0x0 : printf("not supplied"); break;
			case 0x1 : printf("encodes current position data"); break;
			case 0x2 : printf("encodes media catalog number"); break;
			case 0x3 : printf("encodes ISRC"); break;
			default  : printf("encoding marked for reserved type (%d)", addr);
		}
		printf("\n");

		pos += 8;
	}
}


void dump_raw_toc_pma_data(uint8_t *toc) {
	int pos, data_length;
	int cur_session, session, cntrl, addr, point;
	int min, sec, frame, pmin, psec, pframe, nwa, lba, extent;

	data_length = toc[1] | (toc[0] << 8);
	pos = 4; cur_session = 0;
	while (pos < data_length + 2) {
		session = read_cd_bcd(toc[pos+ 0]);
		cntrl   = toc[pos+1] & 15;
		addr    = toc[pos+1] >> 4;
		//tno     = read_cd_bcd(toc[pos+ 2]);
		point   = read_cd_bcd(toc[pos+ 3]);
		min     = read_cd_bcd(toc[pos+ 4]);
		sec     = read_cd_bcd(toc[pos+ 5]);
		frame   = read_cd_bcd(toc[pos+ 6]);
		pmin    = read_cd_bcd(toc[pos+ 8]);
		psec    = read_cd_bcd(toc[pos+ 9]);
		pframe  = read_cd_bcd(toc[pos+10]);
		if (session != cur_session) {
			if (session) printf("\tSession %d\n", session);
			cur_session = session;
		}
		/* if (tno == 0 && session) { */
		if (1) {
			switch (addr) {
				case 1 :
					printf("\t\t");
					switch (point) {
						case 0xa0 :
							printf("Disc type           %s\n\t\t", print_TOC_disc_type(psec));
							printf("First track number  %d\t\t\t", pmin);
							break;
						case 0xa1 :
							printf("Last track number   %d\t\t\t", pmin);
							break;
						case 0xa2 :
							lba  = cd_msf2lba(0, pmin, psec, pframe);
							printf("Lead out            %02d:%02d.%02d (LBA %8d)", pmin, psec, pframe, lba);
							break;
						default :
							lba  = cd_msf2lba(0, pmin, psec, pframe);
							printf("Track start         %02d:%02d.%02d (LBA %8d)", pmin, psec, pframe, lba);
					}
					if (min || sec || frame)
						printf("\n\t\tATIME (%02d:%02d.%02d)\t\t\t", min, sec, frame);
					break;
				case 2 :
					printf("\t\t");
					printf("Media catalog number present\n");
					break;
				case 3 :
					printf("\t\t");
					printf("International Standard Recording Code (ISRC) present\n");
					break;
				case 5 :
					printf("\t\t");
					switch (point) {
						case 0xb0 :
							nwa    = cd_msf2lba(0, min,  sec,  frame);
							extent = cd_msf2lba(0,pmin, psec, pframe);
							printf("Next writable at    %02d:%02d.%02d (LBA %8d)", min, sec, frame, nwa);
							printf("\n\t\t");
							printf("Lead out max at     %02d:%02d.%02d (LBA %8d)", pmin, psec, pframe, extent);
							/* printf("\n\t\t"); */
							/* printf("Disc surface used for %2d %%\t\t", (int) (100.0 * (float) nwa / (float) extent)); */
							break;
						case 0xc0 :
							printf("Start first lead-in %02d:%02d.%02d (LBA %8d)", pmin, psec, pframe, cd_msf2lba(0, pmin, psec, pframe));
							printf("\n\t\t");
							printf("Optimum recording power %d\t\t", min);
							break;
						case 0xc1 :
							printf("Copy of A1 point from ATIP\t\t");
							break;
						default :
							printf("<undumped %d, %d, %02x pair>\t\t", addr, cntrl, point);
							break;
					}
					break;
				default :
					printf("\t\t<undumped %d, %d, %02x pair>\t\t", addr, cntrl, point);
					break;
			}
			printf("\t (");
			if ((cntrl & 12) == 4) {
				printf("data track ");
				if (cntrl & 1) printf("; incremental  "); else printf("; uninterrupted");
			} else {
				printf("audio track");
				if (cntrl & 1) printf("; pre-emphasis of 50/15 microseconds"); else printf("; no pre-emphasis");
			}
			if (cntrl & 2) printf("; copy prohibited");
			printf(")\n");
		}
		pos += 11;
	}
}


char *atip_speed(int speed) {
	static char scrap[100];
	switch (speed) {
		case 0:
			return "Reserved";
		case 1:
			return "2x";
		case 2:
			return "4x";
		case 3:
			return "8x";
		case 4:
			return "16x";
		case 5:
			return "32x";
		case 6:
			return "52x(?)";
		case 7:
		default:
			sprintf(scrap, "(%d)", speed);
	}
	return scrap;
}


void dump_atip_data(uint8_t *toc) {
	int pos, data_length;
	int writing_power, ref_speed, URU, disc_cdrw, disc_subtype, A1, A2, A3;
	int lowest_clv_rec, highest_clv_rec;
	int power_multi, target_y, rec_EW_ratio, hicap_cdr_min;
	int min, sec, frame, pmin, psec, pframe;

	data_length = toc[1] | (toc[0] << 8);
	pos = 4;

	writing_power   = (toc[pos   ] >> 4) & 15;
	ref_speed       =  toc[pos   ] & 7;
	URU             =  toc[pos+ 1] & 64;
	disc_cdrw       =  toc[pos+ 2] & 64;
	disc_subtype    = (toc[pos+ 2] >> 3) & 7;
	A1              =  toc[pos+ 2] & 4;
	A2              =  toc[pos+ 2] & 2;
	A3              =  toc[pos+ 2] & 1;
	min             = read_cd_bcd(toc[pos+ 4]);
	sec             = read_cd_bcd(toc[pos+ 5]);
	frame           = read_cd_bcd(toc[pos+ 6]);
	pmin            = read_cd_bcd(toc[pos+ 8]);
	psec            = read_cd_bcd(toc[pos+ 9]);
	pframe          = read_cd_bcd(toc[pos+10]);
	lowest_clv_rec  = (toc[pos+12] >> 4) & 7;
	highest_clv_rec = (toc[pos+12]     ) & 15;
	power_multi     = (toc[pos+13] >> 4) & 7;
	target_y        = (toc[pos+13] >> 1) & 7;
	rec_EW_ratio    = (toc[pos+14] >> 4) & 7;
	hicap_cdr_min   =  toc[pos+14] & 15;

	printf("\tS1 values\n");
	printf("\t\tDisc type\t\t\t%s\n", disc_cdrw ? "CD-RW":"CD-R");
	printf("\t\tDisc sub-type\t\t\t%d (media sensitivity/speed indicator)\n", disc_subtype);
	printf("\t\tIndicative Target Writing Power\t%d\n", writing_power);
	printf("\t\tDisc is%s in unrestricted use\n", URU ? "":" NOT");
	printf("\t\tReference speed\t\t\t%s\n", atip_speed(ref_speed));
	printf("\tS2 values\n");
	printf("\t\tStart time of lead-in   %02d:%02d.%02d (LBA %8d)\n", min, sec, frame, cd_msf2lba(0, min, sec, frame));
	printf("\tS3 values\n");
	if (hicap_cdr_min == 0) {
		printf("\t\tLast possible lead-out  %02d:%02d.%02d (LBA %8d)\n", pmin, psec, pframe, cd_msf2lba(0, pmin, psec, pframe));
	} else {
		printf("\t\tStart of hicap CD-R     %02d:%02d.%02d (LBA %8d)\n", pmin, psec, pframe, cd_msf2lba(0, pmin, psec, pframe));
		printf("\t\tExtra time in minutes   %d (not sure if correct)\n", hicap_cdr_min);
		printf("\t\tResulting last lead-out %02d:%02d.%02d (LBA %8d)\n", pmin + hicap_cdr_min, psec, pframe, cd_msf2lba(0, pmin + hicap_cdr_min, psec, pframe));
	}
	if (A1) {
		printf("\tA1 values\n");
		printf("\t\tLowest CLV speed\t%s\n", atip_speed(lowest_clv_rec));
		printf("\t\tHighest CLV speed\t%s\n", atip_speed(highest_clv_rec));
		printf("\t\tPower multiplication factor\t\t\t%d\n", power_multi);
		printf("\t\tTarget y value of Modulation/Power function\t%d\n", target_y);
		printf("\t\tRecommended erase/write power ration\t\t%d\n", rec_EW_ratio);
	} else {
		printf("\tA1 values are not valid\n");
		printf("\t\tLowest and highest CLV speed information not specified\n");
		printf("\t\tNo power recommendations\n");
	}
	if (A2) {
		printf("\tA2 values\n");
		printf("\t\t0x%02x 0x%02x 0x%02x\n", toc[pos+16], toc[pos+17], toc[pos+18]);
	} else {
		printf("\tA2 values not valid\n");
	}
	if (A3) {
		printf("\tA3 values\n");
		printf("\t\t0x%02x 0x%02x 0x%02x\n", toc[pos+20], toc[pos+21], toc[pos+22]);
	} else {
		printf("\tA3 values not valid\n");
	}
	if (data_length > 4+23) {
		printf("\tS4 values\n");
		printf("\t\t0x%02x 0x%02x 0x%02x\n", toc[pos+24], toc[pos+25], toc[pos+26]);
	} else {
		printf("\tS4 values not transfered\n");
	}

	printf("\n");
}


void dump_raw_toc_pma_atip_info(void) {
	scsicmd		 cmd;
	uint8_t		 toc[10000];
	int		 toc_len;
	int		 data_length;
	int		 first_session;
	int		 error;

	printf("\nRaw TOC\n");
	printf("\tLba numbers are indicative only\n");

	/* return only fixed part or multi-session info */
	toc_len = 4;

	bzero(cmd, SCSI_CMD_LEN);
	cmd[0] = 0x43;			/* READ TOC/PMA/ATIP INFO		*/
	cmd[1] = 0;			/* LBA's preferably (not relevant)	*/
	cmd[2] = 1;			/* format 1; multi-session info		*/
	cmd[7] = (toc_len >>  8) & 0xff;
	cmd[8] = (toc_len      ) & 0xff;
	cmd[9] = 0;			/* control				*/
	error = uscsi_command(SCSI_READCMD, &dev, cmd, 10, toc, toc_len, 10000, NULL);
	if (error) {
		fprintf(stderr, "TOC reading of multi-session info failed : %s\n", strerror(error));
		fprintf(stderr, "\tassuming first session is sesion 1\n");
		first_session = 1;
	} else {
		/* XXX or just toc[2] ? */
		first_session = read_cd_hex2(toc[2]);
	}

	/* return only fixed part of raw TOC */
	toc_len = 4;

	bzero(cmd, SCSI_CMD_LEN);
	cmd[0] = 0x43;			/* READ TOC/PMA/ATIP INFO		*/
	cmd[1] = 2;			/* officially no LBA's are defined	*/
	cmd[2] = 2;			/* format 2; raw TOC			*/
	cmd[6] = first_session;		/* start at first session		*/
	cmd[7] = toc_len >> 8;
	cmd[8] = toc_len & 0xff;
	cmd[9] = 0;			/* control				*/
	error = uscsi_command(SCSI_READCMD, &dev, cmd, 10, toc, toc_len, 10000, NULL);
	if (error) {
		fprintf(stderr, "TOC reading of fixed part of raw TOC failed : %s\n", strerror(error));
	} else {
		/* read in complete raw TOC */
		data_length = toc[1] | (toc[0] << 8);
		toc_len = data_length + 2;	/* don't count length */

		/* patch for ATAPI: make it even length */
		if (toc_len & 1)
			toc_len++;

		cmd[7] = toc_len >> 8;
		cmd[8] = toc_len & 0xff;
		error = uscsi_command(SCSI_READCMD, &dev, cmd, 10, toc, toc_len, 10000, NULL);
		if (error) {
			fprintf(stderr, "TOC reading of complete raw TOC failed : %s\n", strerror(error));
		} else {
			dump_raw_toc_pma_data(toc);
		}
	}

	printf("\nPMA\n");
	printf("\tLba numbers are indicative only\n");

	/* return only fixed part of PMA */
	toc_len = 4;

	bzero(cmd, SCSI_CMD_LEN);
	cmd[0] = 0x43;			/* READ TOC/PMA/ATIP INFO		*/
	cmd[1] = 2;			/* officially no LBA's are defined	*/
	cmd[2] = 3;			/* format 3; PMA			*/
	cmd[6] = first_session;		/* start at first session		*/
	cmd[7] = toc_len >> 8;
	cmd[8] = toc_len & 0xff;
	cmd[9] = 0;			/* control				*/
	error = uscsi_command(SCSI_READCMD, &dev, cmd, 10, toc, toc_len, 10000, NULL);
	if (error) {
		fprintf(stderr, "Reading of fixed part of PMA failed : %s\n", strerror(error));
	} else {
		/* read in complete PMA */
		data_length =toc[1] | (toc[0] << 8);
		toc_len = data_length + 2;

		/* patch for ATAPI: make it even length */
		if (toc_len & 1)
			toc_len++;

		cmd[7] = toc_len >> 8;
		cmd[8] = toc_len & 0xff;
		error = uscsi_command(SCSI_READCMD, &dev, cmd, 10, toc, toc_len, 10000, NULL);
		if (error) {
			fprintf(stderr, "Reading of complete PMA failed : %s\n", strerror(error));
		} else {
			dump_raw_toc_pma_data(toc);
		}
	}

	printf("\nATIP\n");
	printf("\tLba numbers are indicative only\n");

	/* read in fixed part of response format 0100b */
	toc_len = 4;

	bzero(cmd, SCSI_CMD_LEN);
	cmd[0] = 0x43;			/* READ TOC/PMA/ATIP INFO		*/
	cmd[1] = 2;			/* officially no LBA's are defined	*/
	cmd[2] = 4;			/* format 4; ATIP			*/
	cmd[6] = first_session;		/* start at first session		*/
	cmd[7] = toc_len >> 8;
	cmd[8] = toc_len & 0xff;
	cmd[9] = 0;			/* control				*/
	error = uscsi_command(SCSI_READCMD, &dev, cmd, 10, toc, toc_len, 10000, NULL);
	if (error) {
		fprintf(stderr, "Reading of fixed part of ATIP addition information failed : %s\n", strerror(error));
	} else {
		/* read in complete format 0100b */
		data_length =toc[1] | (toc[0] << 8);
		toc_len = data_length + 4;

		/* patch for ATAPI: make it even length */
		if (toc_len & 1)
			toc_len++;

		cmd[7] = toc_len >> 8;
		cmd[8] = toc_len & 0xff;
		error = uscsi_command(SCSI_READCMD, &dev, cmd, 10, toc, toc_len, 10000, NULL);
		if (error) {
			fprintf(stderr, "Reading of complete ATIP additional information failed : %s\n", strerror(error));
		} else {
			dump_atip_data(toc);
		}
	}
}


void dump_toc_pma_atip_info(void) {
	dump_formatted_toc();
	dump_raw_toc_pma_atip_info();
}


char *prchange(int val) {
	if (val) return "[changeable]";
	return          "[fixed]    ";
}


void dump_parameter_page(uint8_t *pos, uint8_t *change) {
	int page_code, page_length;
	int write_type, track_mode, data_block_type, link_size;
	int testwrite, linksize_valid, zerolinking;
	int appl_code, session_format, packet_size, audio_pause;
	int multi_session, fixed_packets, copybit;
	uint32_t recovery_time_limit, inactiv_time_mult, S_per_MSF, F_per_MSF;
	uint32_t idle_timer_value, standby_timer_value, interval_timer_value;
	uint32_t group1_timeouts, group2_timeouts;
	int i, j, c, per_emcdr;

	page_code    = pos[0] & 63;
	page_length  = pos[1];

	printf("\tGot page 0x%02x (%spersistent) : %2d bytes for ", page_code, pos[0] & 128? "":"non ", page_length);
	switch (page_code) {
		case 0x01 :
			printf("read/write error recovery page\n");
			printf("\t\tError recovery behaviour %s %d\n", prchange(change[2]), pos[2]);
			printf("\t\t\tAutomatic Write Reallocation Enabled (AWRE) %s %s\n", prchange(change[2] & 128), pos[2] & 128 ? "On":"Off");
			printf("\t\t\tAutomatic Read Reallocation Enabled (ARRE)  %s %s\n", prchange(change[2] &  64), pos[2] &  64 ? "On":"Off");
			printf("\t\tError recovery parameter %s %d\n", prchange(change[2] & 63), pos[2] & 63);
			printf("\t\t\tTransfer Block (TB)                         %s %s\n", prchange(change[2] &  32), pos[2] &  32 ? "On":"Off");
			printf("\t\t\tRead Continuous (RC)                        %s %s\n", prchange(change[2] &  16), pos[2] &  16 ? "On":"Off");
			printf("\t\t\t<Reserved>                                  %s %s\n", prchange(change[2] &   8), pos[2] &   8 ? "On":"Off");
			printf("\t\t\tPost Error (PER)                            %s %s\n", prchange(change[2] &   4), pos[2] &   4 ? "On":"Off");
			printf("\t\t\tDisable Transfer on Error (DTE)             %s %s\n", prchange(change[2] &   2), pos[2] &   2 ? "On":"Off");
			printf("\t\t\tDisable Correction (DCR)                    %s %s\n", prchange(change[2] &   1), pos[2] &   1 ? "On":"Off");
			printf("\t\tEnhanced Media Certification and Defect Reporting %s %d\n", prchange(change[7] & 3), pos[7] & 3);
			per_emcdr = (pos[2] & 4) + (pos[7] & 3);
			if (per_emcdr == 0)
				printf("\t\t\tNo certification of medium on read operations and it will not report recovered errors\n");
			if (per_emcdr >=1)
				printf("\t\t\tCertification of medium on read and on verify operation enabled; ");
			if (per_emcdr == 1)
				printf("shall not report recovered error\n");
			if (per_emcdr == 2)
				printf("shall report recovered error or unrecovered error on verify operation.\n");
			if (per_emcdr == 3)
				printf("shall report recovered error or unrecovered error on read operation and verify operation\n");
			if (per_emcdr == 4)
				printf("shall report recovered error if higher lever error correction is used\n");
			if (per_emcdr >= 5)
				printf("shall report recovered error as RECOVERED ERROR/RECOVERED DATA - RECOMMEND REASSIGNMENT\n");
			printf("\t\tRead retry count         %s %d\n", prchange(change[3]), pos[3]);
			printf("\t\tWrite retry count        %s %d\n", prchange(change[8]), pos[8]);
			recovery_time_limit = pos[11] + (pos[10] << 8);
			printf("\t\tRecovery time limit      %s %d\n", prchange(change[11]), recovery_time_limit);
			printf("\n");
			break;
		case 0x03 :
			printf("MRW mode page\n");
			printf("\t\t`%s' LBA space selected\n", pos[3] & 1 ? "General Application Area" : "Defect Managed Area");
			printf("\n");
			break;
		case 0x05 :
			printf("write parameter page\n");
			write_type	= pos[2] & 15;
			track_mode	= pos[3] & 15;
			data_block_type	= pos[4] & 15;
			testwrite	= pos[2] & 16;
			linksize_valid	= pos[2] & 32;
			zerolinking	= pos[2] & 64;
			multi_session   = pos[3] >> 6;
			fixed_packets	= pos[3] & 32;
			copybit		= pos[3] & 16;
			link_size	= pos[5];
			appl_code	= pos[7] & 63;
			session_format  = pos[8];
			packet_size	= pos[13] + (pos[12] << 8) + (pos[11] << 16) + (pos[10] << 24);
			audio_pause	= pos[15] + (pos[14] << 8);

			printf("\t\tWrite type          %s %s\n", prchange(change[2] & 15), print_write_type(write_type));
			printf("\t\tTrack mode          %s %d (mode 1 Q subchannel control nibble)\n", prchange(change[3] & 15), track_mode);
			printf("\t\tData block type     %s %s\n", prchange(change[4] & 15), print_data_block_type(data_block_type));
			printf("\t\tTestwriting         %s %s\n", prchange(change[2] & 16), testwrite ? "On":"Off");
			printf("\t\tLinksize            %s %d sectors (%s)\n", prchange(change[5]), link_size, linksize_valid ? "valid": "not valid (7)");
			printf("\t\tZerolinking support %s %s\n", prchange(change[2] & 64), zerolinking ? "On":"Off");
			printf("\t\tApplication code    %s %d\n", prchange(change[7] & 63), appl_code);
			printf("\t\tSession format      %s %s\n", prchange(change[8]), print_session_format(session_format));
			printf("\t\tUsing fixed packets %s %s\n", prchange(change[3] & 32), fixed_packets ? "On":"Off");
			printf("\t\tCopybit             %s %s\n", prchange(change[3] & 16), copybit ? "On":"Off");
			printf("\t\tMulti session field %s %d\n", prchange(change[3] >> 6), multi_session);
			printf("\t\tPacket size         %s %d\n", prchange(change[13]), packet_size);
			printf("\t\tAudio pause length  %s %d\n", prchange(change[15]), audio_pause);
			printf("\t\tMedia catalog number                 %s 0x", prchange(change[31]));
			for (i = 31; i >= 16; i--) printf("%02x", pos[i]);
			printf("\n");
			printf("\t\tInitiational standard recording code %s 0x", prchange(change[47]));
			for (i = 47; i >= 32; i--) printf("%02x", pos[i]);
			printf("\n");
			printf("\t\tSubheader byte 0    %s 0x%02x\n", prchange(change[48]), pos[48]);
			printf("\t\tSubheader byte 1    %s 0x%02x\n", prchange(change[49]), pos[49]);
			printf("\t\tSubheader byte 2    %s 0x%02x\n", prchange(change[50]), pos[50]);
			printf("\t\tSubheader byte 3    %s 0x%02x\n", prchange(change[51]), pos[51]);
			printf("\n");
			break;
		case 0x07 :
			printf("verify error recovery mode page\n");
			printf("\t\tNot dumped yet. (And not permitted for MM LUs)\n");
			printf("\n");
			break;
		case 0x08 :
			printf("caching mode page\n");
			printf("\t\tWrite cache         %s %s\n", prchange(change[2] & 4), pos[2] & 4 ?  "enabled" : "disabled");
			printf("\t\tRead  cache         %s %s\n", prchange(change[2] & 1), pos[2] & 1 ? "disabled" :  "enabled");
			printf("\n");
			break;
		case 0x0b :
			printf("media types supported page\n");
			printf("\t\tNot dumped yet. (And not permitted for MM LUs)\n");
			printf("\n");
			break;
		case 0x0d :
			printf("cd device parameters page\n");
			inactiv_time_mult = pos[3] & 15;
			S_per_MSF = pos[5] + (pos[4] << 8);
			F_per_MSF = pos[7] + (pos[6] << 8);
			printf("\t\tHold track inactivity time  %s value %d (%s)\n", prchange(change[3] & 15), inactiv_time_mult, print_inactivity_time(inactiv_time_mult));
			printf("\t\tSeconds per MSF             %s %d\n", prchange(change[5]), S_per_MSF);
			printf("\t\tFrames  per MSF             %s %d\n", prchange(change[7]), F_per_MSF);
			printf("\n");
			break;
		case 0x0e :
			printf("cd audio control page\n");
			printf("\t\tStop on track crossing      %s %s\n", prchange(change[ 2] & 2 ), pos[2] & 2 ? "On":"Off");
			printf("\t\tCDDA output port 0 channel  %s %d\n", prchange(change[ 8] & 31), pos[ 8] & 31);
			printf("\t\tOutput port 0 volume        %s %d\n", prchange(change[ 9]     ), pos[ 9]);
			printf("\t\tCDDA output port 1 channel  %s %d\n", prchange(change[10] & 31), pos[10] & 31);
			printf("\t\tOutput port 1 volume        %s %d\n", prchange(change[11]     ), pos[11]);
			printf("\t\tCDDA output port 2 channel  %s %d\n", prchange(change[12] & 31), pos[12] & 31);
			printf("\t\tOutput port 2 volume        %s %d\n", prchange(change[13]     ), pos[13]);
			printf("\t\tCDDA output port 3 channel  %s %d\n", prchange(change[14] & 31), pos[14] & 31);
			printf("\t\tOutput port 3 volume        %s %d\n", prchange(change[15]     ), pos[15]);
			printf("\n");
			break;
		case 0x1a :
			printf("power condition page\n");
			idle_timer_value    = pos[ 7] + (pos[ 6] << 8) + (pos[ 5] << 16) + (pos[ 4] << 24);
			standby_timer_value = pos[11] + (pos[10] << 8) + (pos[ 9] << 16) + (pos[ 8] << 24);
			printf("\t\tIdle timer activate         %s %s\n", prchange(change[3] & 2  ), pos[3] & 2 ? "On":"Off");
			printf("\t\tStandby timer active        %s %s\n", prchange(change[3] & 1  ), pos[3] & 1 ? "On":"Off");
			printf("\t\tIdle timer start value      %s %d * 100ms\n", prchange(change[ 7] & 1 ), idle_timer_value);
			printf("\t\tStandby timer start value   %s %d * 100ms\n", prchange(change[11] & 1 ), standby_timer_value);
			printf("\n");
			break;
		case 0x1c :
			printf("informational exceptions control page (not checked yet)\n");
			interval_timer_value = pos[ 7] + (pos[ 6] << 8) + (pos[ 5] << 16) + (pos[ 4] << 24);
			printf("\t\tLogging                                     %s %s\n", prchange(pos[2] & 1), pos[2] & 1 ? "standard" : "vendor specific");
			printf("\t\tTest device failure notification generation %s %s\n", prchange(pos[2] & 4), pos[2] & 4 ? "On" : "Off");
			printf("\t\tInterval timer if enabled                   %s %d * 100ms (?)\n", prchange(pos[7] & 1), interval_timer_value);
			printf("\t\tRest undumped\n");
			printf("\n");
			break;
		case 0x1d :
			printf("timeout and protect page\n");
			group1_timeouts = pos[7] + (pos[6] << 8);
			group2_timeouts = pos[9] + (pos[8] << 8);
			printf("\t\tTimeouts commands enabled   %s %s\n", prchange(change[4] & 4), pos[4] & 4 ? "On":"Off");
			printf("\t\tDisable device till powerd. %s %s\n", prchange(change[4] & 2), pos[4] & 2 ? "On":"Off");
			printf("\t\tSoftware write protect      %s %s\n", prchange(change[4] & 1), pos[4] & 1 ? "On":"Off");
			if (page_length >= 8)
				printf("\t\tGroup 1 minimum time outs   %s %d\n", prchange(change[7]), group1_timeouts);
			if (page_length >= 10)
				printf("\t\tGroup 2 minimum time outs   %s %d\n", prchange(change[9]), group2_timeouts);
			printf("\n");
			break;
		case 0x2a :
			printf("CD/DVD capabilities and mechanical status page\n");
			printf("\t\tNot dumped yet\n");
			printf("\n");
			break;
		default :
			/* dump bytes in hex:char notation */
			printf("Reserved/vendor specific\n");
			printf("\t\t    ");
			for (j = 0; j < 16; j++) {
				printf("%02x ", j);
			}
			printf("  ASCII\n");
			for (i = 2; i < page_length+2; i+= 16) {
				printf("\t\t%02x  ", i-2);
				for (j = 0; j < 16; j++) {
					if (i+j < page_length+2) {
						printf("%02x ", pos[i+j]);
					} else {
						printf("   ");
					}
				}
				printf(": ");
				for (j = 0; j < 16; j++) {
					if (i+j < page_length+2) {
						c = pos[i+j];
						if (c < 32 || c == 127)
							c = '.';
						printf("%c", c);
					} else {
						printf(" ");
					}
				}
				printf("\n");
			}
			printf("\n");
			break;
	}
}


#define max_blk_len 60000
void dump_parameter_pages(void) {
	scsicmd cmd;
	int     blk_len, val_length, changeable_length;
	uint8_t val[max_blk_len], changeable[max_blk_len], zeros[256];
	int	voff, choff, vpage_code, vpage_length, chpage_code, chpage_length;
	int	error;

	bzero(val, max_blk_len);
	bzero(changeable, max_blk_len);
	bzero(zeros, 256);

	/* read the block size */
	blk_len = 8;
	bzero(cmd, SCSI_CMD_LEN);
	cmd[0] = 0x5A;			/* MODE SENSE (10)		*/
	cmd[1] = 0;			/* allow multiple blocks	*/
	cmd[2] = 0x3F;			/* return all current values	*/
	cmd[7] = blk_len >> 8;		/* MSB block length 		*/
	cmd[8] = blk_len & 0xff;	/* LSB block length 		*/
	cmd[9] = 0;			/* control			*/
	error = uscsi_command(SCSI_READCMD, &dev, cmd, 10, val, blk_len, 10000, NULL);
	if (error) {
		fprintf(stderr, "While reading all parameter pages : %s\n", strerror(error));
		return;
	}
	val_length = val[1] | (val[0] << 8);
	printf("\tSCSI mode sense for all pages returned %d bytes of data\n", val_length);

	/* read the mode sense block */
	blk_len = val_length;
	cmd[7] = blk_len >> 8;		/* MSB block length 		*/
	cmd[8] = blk_len & 0xff;	/* LSB block length 		*/
	error = uscsi_command(SCSI_READCMD, &dev, cmd, 10, val, blk_len, 10000, NULL);
	if (error) {
		fprintf(stderr, "While reading all parameter pages : %s\n", strerror(error));
		return;
	}

	cmd[2] = 64 | 0x3F;		/* get all changeable values	*/
	error = uscsi_command(SCSI_READCMD, &dev, cmd, 10, changeable, blk_len, 10000, NULL);
	if (error) {
		fprintf(stderr, "While reading all changeable parameter pages : %s\n", strerror(error));
	}
	changeable_length = changeable[1] | (changeable[0] << 8);
	printf("\tSCSI mode sense changeable parameters list returned %d bytes of data\n", changeable_length);

	printf("\n");

	/* read the mode sense block and process page by page               */
	voff = 8;
	while (voff < val_length) {
		vpage_code    = val[voff    ] & 63;
		vpage_length  = val[voff + 1];

		if (vpage_code) {
			choff = 8; chpage_code = -1;
			while (choff < changeable_length) {
				chpage_code   = changeable[choff    ] & 63;
				chpage_length = changeable[choff + 1];
				if (chpage_code == vpage_code) break;
				choff += chpage_length + 2;
			}
			if (chpage_code == vpage_code) {
				dump_parameter_page(val + voff, changeable + choff);
			} else {
				printf("\tWarning: no matching changeable bits page was found\n");
				dump_parameter_page(val + voff, zeros);
			}
		}
		voff += vpage_length + 2;
	}
}
#undef max_blk_len


int main(int argc, char **argv) {
	scsicmd cmd;
	uint8_t buf[36];
	struct uscsi_addr saddr;
	int drive_type;
	int error;

	bzero(&dev, sizeof(dev));

	if (argc != 2) {
		printf("Usage : %s devicename\n", argv[0]);
		return 1;
	}

	/* Open the device */
	dev.dev_name = strdup(argv[1]);
	printf("Opening device %s\n", dev.dev_name);
	if (uscsi_open(&dev) != 0) {
		exit(1);
	}

	error = uscsi_check_for_scsi(&dev);
	if (error) {
		fprintf(stderr, "sorry, not a SCSI device : %s\n", strerror(error));
		exit(1);
	}

	error = uscsi_identify(&dev, &saddr);
	if (error) {
		fprintf(stderr, "SCSI identify returned : %s\n", strerror(error));
		exit(1);
	}

	printf("\n\nDevice attachment identifies itself as : ");

	switch (saddr.type) {
		case USCSI_TYPE_SCSI    :
			printf("SCSI   busnum = %d, target = %d, lun = %d\n",
				saddr.addr.scsi.scbus, saddr.addr.scsi.target, saddr.addr.scsi.lun);
			break;
		case USCSI_TYPE_ATAPI   :
			printf("ATAPI  busnum = %d, drive = %d\n",
				saddr.addr.atapi.atbus, saddr.addr.atapi.drive);
			break;
		case USCSI_TYPE_UNKNOWN :
			printf("Unknown attachment\n");
	}

	printf("\n\nTest unit ready\n");
	bzero(cmd, SCSI_CMD_LEN);
	cmd[0] = 0;	/* test unit ready */
	error = uscsi_command(SCSI_READCMD, &dev, cmd, 6, buf, 0, 10000, NULL);
	if (error) perror("SCSI test unit ready returned : ");

	printf("Device identifies itself as : ");
	dump_drive_identify(&drive_type);

	printf("\n\nCURRENT features\n");
	dump_drive_configuration(1);

	printf("\n\nCAPAPLE features\n");
	dump_drive_configuration(0);

	printf("\n\nRead recorded capacity\n");
	dump_recorded_capacity();

	if (drive_type == DEVICE_TYPE_MMC) {
		printf("\n\nDisc information\n");
		dump_disc_information();
	}

	printf("\n\nFormat capabilities\n");
	dump_format_capabilities();

	if (drive_type == DEVICE_TYPE_MMC) {
		printf("\n\nReading TOC/PMA/ATIP\n");
		dump_toc_pma_atip_info();
	}

	if (drive_type == DEVICE_TYPE_MMC) {
		/* normally support mode sense 10 */
		printf("\n\nReading SCSI 'mode sense' parameter pages\n");
		dump_parameter_pages();
	} else {
		printf("\n\nnot an MMC class, `mode sense' 10 parameters might fail\n");
		dump_parameter_pages();
	}

	uscsi_close(&dev);

	return 0;
}

