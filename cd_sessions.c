/* $NetBSD$ */

/*
 * File "cd_sessions.c" is part of the UDFclient toolkit.
 * File $Id: cd_sessions.c,v 1.34 2011/02/01 20:43:40 reinoud Exp $ $Name:  $
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
#include <sys/types.h>

#include "udf.h"
#include "udf_bswap.h"


struct udf_discinfo *disc;
extern int udf_verbose;
extern int uscsilib_verbose;


extern void udf_dump_discinfo(struct udf_discinfo *disc);

int main(int argc, char **argv) {
	char *dev_name;
	int   error;

	if (argc != 2) {
		printf("Usage : %s devicename\n", argv[0]);
		return 1;
	}

	dev_name         = argv[1];
	udf_verbose      = UDF_VERBLEV_ACTIONS;
	udf_verbose      = UDF_VERBLEV_MAX;
	uscsilib_verbose = 0;

	printf("Opening device %s\n", dev_name);
	error = udf_open_disc(dev_name, /* discop_flags */ 0, &disc);
	if (error) {
		fprintf(stderr, "Can't open my device : %s\n", strerror(error));
		exit(1);
	}

	udf_dump_discinfo(disc);

	udf_close_disc(disc);

	return 0;
}

