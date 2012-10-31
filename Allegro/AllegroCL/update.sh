#! /bin/sh
# Run ./update.sh to rebuild all images to include patches.

acldir="`dirname $0`"
if test "$acldir" != "."; then
    echo cd $acldir
    cd $acldir
fi

set -eu

usage ()
{
    cat << EOF
usage: update.sh [-u] [--proxy proxy] [--proxy-auth auth]
                 [--newspace size] [--oldspace size] 
                 [--lisp-heap-start addr] [--lisp-heap-size size] 
                 [--aclmalloc-heap-start addr] [--aclmalloc-heap-size size] 

Without any arguments, rebuild images to contain all downloaded patches.
With the -u argument, download new updates before rebuilding images.

--proxy allows specification of a proxy host and port.  It is typically 
something like "cobweb:3128".

--proxy-auth allows specification of proxy basic authentication.  It is
typically your password for the proxy.

You may specify --proxy without --proxy-auth.

The --*-heap-size arguments must be one of

  size := number + suffix
  number := [0-9]+
  suffix := m | k

The "m" and "k" in the suffix stand for "megabytes" and "kilobytes".

The --newspace, --oldspace and --*-heap-start arguments use
the same format as the --*-heap-size arguments above.

More information can be obtained from 
  http://www.franz.com/support/documentation/current/doc/building-images.htm
EOF
    exit 1
}

download=
proxy=
proxyauth=
host="www.franz.com"

while test $# -gt 0; do
    case $1 in
	--help) usage ;;
	-u) download=xxx ;;
	--c-heap-start|--aclmalloc-heap-start)
	    shift
	    if test $# -eq 0; then usage; fi
	    export ACL_BUILD_ACLMALLOC_HEAP_START=$1
	    ;;
	--c-heap-size|--aclmalloc-heap-size)
	    shift
	    if test $# -eq 0; then usage; fi
	    export ACL_BUILD_ACLMALLOC_HEAP_SIZE=$1
	    ;;
	--lisp-heap-start)
	    shift
	    if test $# -eq 0; then usage; fi
	    export ACL_BUILD_LISP_HEAP_START=$1
	    ;;
	--lisp-heap-size)
	    shift
	    if test $# -eq 0; then usage; fi
	    export ACL_BUILD_LISP_HEAP_SIZE=$1
	    ;;
	--newspace)
	    shift
	    if test $# -eq 0; then usage; fi
	    export ACL_BUILD_NEWSPACE=$1
	    ;;
	--oldspace)
	    shift
	    if test $# -eq 0; then usage; fi
	    export ACL_BUILD_OLDSPACE=$1
	    ;;
	--proxy)
	    shift
	    if test $# -eq 0; then usage; fi
	    proxy=$1
	    ;;
	--proxy-auth)
	    shift
	    if test $# -eq 0; then usage; fi
	    proxyauth=$1
	    ;;
	--host)
	    shift
	    if test $# -eq 0; then usage; fi
	    host=$1
	    ;;
	*)
	    usage
            exit 1
	    ;;
    esac
    shift
done

if test -n "$download"; then
    echo Will download updates before rebuilding images.
else
    echo Will not download updates before rebuilding images.
fi

if test -n "$proxy"; then
    echo Using proxy $proxy
    if test -n "$proxyauth"; then
	echo "   ...with proxy basic authentication: $proxyauth"
    fi
fi

tempfile=update$$
trap "/bin/rm -f $tempfile" 0

if test -n "$download"; then
    rm -f $tempfile
    cat <<EOF > $tempfile
(sys:update-allegro
  :host "$host"
EOF
    if test -n "$proxy"; then
	cat <<EOF >> $tempfile
  :proxy "$proxy"
EOF
    fi
    if test -n "$proxyauth"; then
	cat <<EOF >> $tempfile
  :proxy-basic-authorization "$proxyauth"
EOF
    fi
    cat <<EOF >> $tempfile
)
EOF
    cat $tempfile
    echo ./alisp -qq -batch -L $tempfile -kill
    ./alisp -qq -batch -L $tempfile -kill
    rm -f $tempfile
fi

rm -f TRIAL NON-TRIAL

./alisp -L update2.cl -qq

do_bu=1

images="alisp.dxl alisp8.dxl mlisp.dxl mlisp8.dxl composer.dxl \
	clim.dxl allegro.dxl allegro-ansi.dxl allegro-express.dxl"

if test ! -f alisp; then
    cp -p alisp alisp.bak
fi

if test -f clim; then
    rm -f clim
    ln -f mlisp clim
fi

if test -f clim8; then
    rm -f clim8
    ln -f mlisp8 clim8
fi

if test -f composer; then
    rm -f composer
    ln -f mlisp composer
fi

if test -f composer8; then
    rm -r composer8
    ln -f mlisp8 composer8
fi

env="env ACL_LOCALE=C ACL_UPDATING_IMAGES=t"

for image in $images; do
    if test ! -f $image; then
	continue
    fi

    base="`echo ${image} | sed 's/.dxl//'`"
    lisp=$base

    if test $do_bu -ne 0; then
	do_bu=0
	echo "Doing bundle check.  Output going to tmpbu.build..."
	echo "The bundle check may take several minutes."
	cat << EOF > $tempfile
(build-lisp-image "tmpbu.dxl" :verbose t :include-ide nil :include-devel-env t :internal-debug "tmpbu.debug")
(exit 0)
EOF
	cat $tempfile | $env ./$lisp -I "$base".dxl -qq -batch -backtrace-on-error 2>&1 > tmpbu.build
	cat << EOF > $tempfile
(when (fboundp 'excl::update-bundle-check)
  (unless (excl::update-bundle-check t)
    (excl::update-bundle-files)))
(exit 0)
EOF
	cat $tempfile | $env ./$lisp -I tmpbu.dxl -qq -batch -backtrace-on-error 2>&1 >> tmpbu.build
	rm -f tmpbu.dxl
	echo "Finished bundle check."
    fi

    if test ! -f $lisp; then
	echo "Sorry, $lisp does not exist."
	exit 1
    fi

    orig_image="$base"orig.dxl
    old_image="$base"old.dxl

    if test ! -f $orig_image; then
	mv $image $orig_image
	chmod a-w $orig_image
	prev=$orig_image
    fi
    if test -f $image; then
	mv $image $old_image
	prev=$old_image
    fi

    if test -f require-search-list.cl; then
	rsl=`cat require-search-list.cl`
    else
	rsl=
    fi

    set +e
    echo "Building $image..."
    cat << EOF > $tempfile
(setq *print-startup-message*
  (let ((r (reverse *print-startup-message*)))
    (reverse
     (pushnew '(:external-format . t) r :test #'equal))))
(when excl::*pll-file*
  (setq excl::*pll-file*
    (or (sys:tmp-mnt-frobber excl::*pll-file*) excl::*pll-file*)))
$rsl
(build-lisp-image "$image" :pll-from-sys t :verbose t :internal-debug "$base.debug")
(exit 0)
EOF
    $env ./$lisp -I $orig_image -qq -batch -backtrace-on-error -L $tempfile 2>&1 > $base.build
    status=$?
    set -e
    if test $status -ne 0; then
	echo "Building $image failed.  Will restore previous image."
	echo "Check $base.build for build errors."
	rm -f $image
	mv $prev $image
	case $image in
# Don't make this a fatal error, since the likely problem
# is the host operating system does not have the proper version of
# GTK2 installed.
	    allegro*) ;;
	    *) exit 1 ;;
	esac
    fi
done

rm -f UPDATEALLEGROPENDING.txt
