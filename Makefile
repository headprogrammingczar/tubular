# Usage: make vm-build

BUILDVM_RUNNING := $(shell vagrant status build | grep -c 'running (virtualbox)')

# provision the build box
ifeq ($(BUILDVM_RUNNING),1)
turn-on-build-vm: ;
else
turn-on-build-vm:
	vagrant up build
endif

# scp source onto build box, do a build, then scp the result back to the host
vm-build: turn-on-build-vm
	vagrant ssh build -c "sudo bash -c 'rm -rf /vagrant && mkdir /vagrant && chown vagrant:vagrant /vagrant'"
	vagrant scp . build:/vagrant
	vagrant ssh build -c "cd /vagrant && stack build"
	rm -rf .stack-work
	vagrant scp build:/vagrant/.stack-work .stack-work

