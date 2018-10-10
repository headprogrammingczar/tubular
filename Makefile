# Usage: make host-build
# requires vagrant and virtualbox, and the vagrant-scp plugin

# scp source onto build box, do a build, then scp the result back to the host
host-build:
	vagrant up build
	vagrant ssh build -c "sudo bash -c 'rm -rf /vagrant \
	    && mkdir /vagrant \
	    && chown vagrant:vagrant /vagrant'"
	vagrant scp . build:/vagrant
	vagrant ssh build -c "cd /vagrant && stack build"
	rm -rf .stack-work
	vagrant scp build:/vagrant/.stack-work .stack-work

host-clean:
	vagrant destroy -f build net-root d f
	rm -rf .stack-work .vagrant

# install, but wherever user input would be needed instead use assumptions about the test network
host-install: 
	vagrant up net-root build

# perform full-stack testing with a variety of configurations
# by taking control of all three vms in the test network
host-test: host-install
	vagrant up d f 

.PHONY: host-build host-clean host-install host-test
