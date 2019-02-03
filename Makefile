# Usage: make build
# requires vagrant and virtualbox, and the vagrant-scp plugin

# this Makefile controls the build environment
# so contributors don't need to set up their own BSD VM
# it also sets up the test environment, which is multiple other VMs

# Makefile.bsd controls the package definition
# it gets copied as "Makefile" outside the source tree

# tubular/Makefile controls compilation
# it also moves the resulting files around so they can be
# easily copied to the bsd packaging staging directory

# the bsd build process is
# make a source dist tarball, put it where make fetch can get it
# then it builds by recursively calling another Makefile
# then it copies files out of build dir to staging dir, structured like chroot
# then it makes a package from staging dir
# non-standard actions should go in pre-* targets

# inner Makefile needs to output in directory structure that can translate to staging dir

VERSION=0.1.0.0
BUILDNUM=1
OSDIR=x86_64-freebsd/lts-11.22
GHCDIR=8.2.2

all: tubular-${VERSION}.txz

build: tubular-${VERSION}.txz

# scp source onto build box, do a build, then scp the result back to the host
# bsd has all the worst luck with support for things
# use the vagrant-scp plugin to extract build output
tubular-${VERSION}.txz:
	vagrant up build
	# workspace prep
	vagrant ssh build -c "sudo bash -c 'rm -rf /vagrant \
	    && mkdir /vagrant \
	    && chown vagrant:vagrant /vagrant'"
	vagrant scp tubular build:/vagrant/tubular-src
	vagrant scp Makefile.bsd build:/vagrant/Makefile
	vagrant scp pkg-descr build:/vagrant
	vagrant scp pkg-plist build:/vagrant
	# build the code
	vagrant ssh build -c "cd /vagrant/tubular-src && stack build"
	# make a "source dist"
	vagrant ssh build -c "bash -c 'cd /vagrant \
	    && mkdir tubular-${VERSION} \
	    && cp -R tubular-src/.stack-work/install/${OSDIR}/${GHCDIR}/* tubular-${VERSION} \
	    && cp tubular-src/Makefile tubular-${VERSION} \
	    && sudo mkdir -p /usr/ports/distfiles \
	    && sudo tar cvzf /usr/ports/distfiles/tubular-${VERSION}.tar.gz tubular-${VERSION} \
	    && make makesum'"
	# build the bsd package
	vagrant ssh build -c "bash -c 'cd /vagrant && make && make package'"
	# copy the final package off the build box
	vagrant scp build:/vagrant/work/pkg/tubular-${VERSION}.txz .

# tbd
#	rm -rf tubular/.stack-work
#	vagrant scp build:/vagrant/tubular/.stack-work tubular/.stack-work

stack-build:
	vagrant ssh build -c "cd /vagrant/tubular && stack build"

bsd-build:
	vagrant ssh build -c "cd /vagrant/tubular \
	    && make stage \
	    && make check-orphans \
	    && make package \
	    && portlint"

# vagrant returns a non-zero status when it destroys a vm
# no idea why, but it can usually be safely ignored
clean:
	vagrant destroy -f build net-root d f || true
	rm -rf tubular/.stack-work .vagrant tubular-*.txz

install: tubular-${VERSION}.txz
	vagrant up net-root
	vagrant scp tubular-${VERSION}.txz net-root:~
	# remove the previously installed version
	vagrant ssh net-root -c "sudo pkg remove -y tubular || true"
	vagrant ssh net-root -c "sudo pkg install -y tubular-${VERSION}.txz"

# perform full-stack testing with a variety of configurations
# by taking control of all three vms in the test network
test: install
	vagrant up d f 

.PHONY: all build clean install test
