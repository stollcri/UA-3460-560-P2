# $Id: runlisp.sh,v 1.12 2004/03/26 15:05:45 layer Exp $
#
# A portable way to run lisp from a makefile.  Needed because lisp.exe
# on Windows can't read from `stdin' and you can't redirect the output
# either.
#
# On Windows, use of this script relies on the GNU-Win32 tools, which are
# a port of the popular GNU development tools to Windows NT and 95.
# See http://www.cygnus.com/misc/gnu-win32/ for more information.
#
# Usage: runlisp [-e] [-i] [-t title] [-o output_file] [-f file | form] ...
# run
#		-e	 ignore the exit status (for printing a message about the errors)
#		-i	 run in `interactive' mode (ie, lisp doesn't exit if an error
#			 occurs)
#		-t	 set the title of the window to `title'
#		-o	 send output to `output_file'.  `-' means no file
#		-f	 read forms from `file'
#		form use `form' to send to lisp
#		...	 other arguments passed to lisp
#
# `-f file | form' must be just before `...'.  -t is ignore on UNIX.
# An output_file of `-' does not work on Windows.

# For debugging uncomment the following line:
#set -x

from_file=
output=
title=
batch="-batch -backtrace-on-error"
ignore_exit_status=xxx
plus_args="+M +B +cn"
form=

while test $# -gt 0; do
	case $1 in
	+p)	plus_args="$plus_args +p"
		;;
	-e)	ignore_exit_status=
		;;
	-f)	shift
		from_file=xxx
		form=$1
		;;
	-i)	batch=
		;;
	-o)	shift
		if test "X$1" != "X-"; then
			output=$1
		fi
		;;
	-t)	shift
		title="$1"
		;;
	*)	break
		;;
	esac
	shift
done

if test -z "$form"; then
	form=$1
	shift
fi

if test -n "$title" -a '(' -d c:/ -o -d //C/ ')'; then
	cl="$1 +t $title"
else
	cl=$1
fi
shift

other_lisp_args="$* -q $batch"

if test -d c:/ -o -d //C/; then # on Windows
# On Windows, we must have an output file, so send it to runlisp.out
# if the user didn't request one.
	if test -z "$output"; then
		output="runlisp.out"
	fi
	other_lisp_args="$other_lisp_args -d $output"
	if test -n "$from_file"; then
		echo $cl $plus_args +s $form $other_lisp_args
		echo "[lisp output redirected to $output]"
		$cl $plus_args +s $form $other_lisp_args
		status=$?
	else
		rm -f runlisp.tmp
		echo $form > runlisp.tmp
		echo $cl $plus_args +s runlisp.tmp $other_lisp_args
		echo "[lisp output redirected to $output]"
		$cl $plus_args +s runlisp.tmp $other_lisp_args
		status=$?
		rm -f runlisp.tmp
	fi
	if test -n "${RUNLISP_SH_SHOW_OUTPUT-}"; then
		cat $output
	fi
## astore uses 124, and it's convenient not to have it flagged as an
## "error"
	if test -n "$ignore_exit_status" -a $status -ne 0 \
			-a $status -ne 123 -a $status -ne 124; then
		echo ""
		echo "==== check \"$output\" for errors and/or warnings ===="
	fi
else # on UNIX
	echo "$cl $other_lisp_args"
	if test -n "$from_file"; then
		start="cat $form"
	else
		start="echo $form"
	fi
	if test -n "$output"; then
		echo "[lisp output redirected to $output]"
		$start | $cl $other_lisp_args > $output 2>&1
		status=$?
		if test -n "$RUNLISP_SH_SHOW_OUTPUT"; then
			cat $output
		fi
	else
		$start | $cl $other_lisp_args
		status=$?
	fi
fi
exit $status
