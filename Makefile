# Usage: make host-build
# requires vagrant and virtualbox

# targets are divided into host-* which run on the host machine,
# and unprefixed targets that run on the build VM

BUILDVM_RUNNING := $(shell vagrant status build | grep -c 'running (virtualbox)')
NETROOTVM_RUNNING := $(shell vagrant status net-root | grep -c 'running (virtualbox)')
DVM_RUNNING := $(shell vagrant status d | grep -c 'running (virtualbox)')
FVM_RUNNING := $(shell vagrant status f | grep -c 'running (virtualbox)')

ifeq ($(BUILDVM_RUNNING),1)
host-power-build-vm: ;
else
host-power-build-vm:
	vagrant up build
endif

ifeq ($(NETROOTVM_RUNNING),1)
host-power-netroot-vm: ;
else
host-power-netroot-vm:
	vagrant up net-root
endif

ifeq ($(DVM_RUNNING),1)
host-power-d-vm: ;
else
host-power-d-vm: host-install
	vagrant up d
endif

ifeq ($(FVM_RUNNING),1)
host-power-f-vm: ;
else
host-power-f-vm: host-install
	vagrant up f
endif

# scp source onto build box, do a build, then scp the result back to the host
host-build: host-power-build-vm
	vagrant ssh build -c "sudo bash -c 'rm -rf /vagrant && mkdir /vagrant && chown vagrant:vagrant /vagrant'"
	vagrant scp . build:/vagrant
	vagrant ssh build -c "cd /vagrant && stack build"
	rm -rf .stack-work
	vagrant scp build:/vagrant/.stack-work .stack-work

host-clean:
	vagrant destroy -f build
	vagrant destroy -f net-root
	vagrant destroy -f d
	vagrant destroy -f f
	rm -rf .stack-work .vagrant

host-install: host-power-netroot-vm host-build

host-test: host-install host-power-d-vm host-power-f-vm

