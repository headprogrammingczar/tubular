# each vm has a nat interface automatically that should only be used for "vagrant ssh"
# a host-only network above net-root for external access
# requires vboxnet0 host-only network with adapter at ip #{hostonly_subnet}.254
# internal networks simulate wires below
# assume subnets are /24 for testing, strings are easier than math
hostonly_subnet = "192.168.56"
internal_subnet = "192.168.57"

# build_mem must be at least 1024 per cpu
# 4 cpus seems to be ideal for build time
build_mem = 4096
build_cpus = 4

Vagrant.configure("2") do |config|
  config.vm.define "build" do |node|
    # bsd has all the worst luck with support for things
    # use the vagrant-scp plugin to extract build output
    node.vm.box = "generic/freebsd11"
    node.vm.provision "shell", path: "vagrant/build-provision.sh"
    node.vm.provision "shell", inline: "echo \"Use the project's Makefile to get started\" >> /etc/motd"

    node.vm.provider "virtualbox" do |v|
      v.memory = build_mem
      v.cpus = build_cpus
    end
  end

  config.vm.define "net-root" do |node|
    node.vm.box = "generic/freebsd11"

    node.vm.network "private_network", ip: "#{hostonly_subnet}.1"
    # TODO bonding these to the internal subnet's gateway ip is done in bash
    # but vagrant expects addresses :/
    node.vm.network "private_network", ip: "#{internal_subnet}.252"
    node.vm.network "private_network", ip: "#{internal_subnet}.253"

    # vbox allows up to 4 interfaces
    #   one is NAT for vagrant access
    #   one is "up" on the router VM
    #   leaving two for VMs "below" the router
    node.vm.provider "virtualbox" do |vb|
      vb.customize ["modifyvm", :id, "--nic2", "hostonly"]
      vb.customize ["modifyvm", :id, "--nic3", "intnet"]
      vb.customize ["modifyvm", :id, "--intnet3", "intnet2"]
      vb.customize ["modifyvm", :id, "--nic4", "intnet"]
      vb.customize ["modifyvm", :id, "--intnet4", "intnet3"]
    end

    node.vm.provision "shell", inline: "echo \"Use the project's Makefile to get started\" >> /etc/motd"
  end
  config.vm.define "d" do |node|
    node.vm.box = "debian/jessie64"
    # the intnet configuration makes it dhcp to net-root
    node.vm.network "private_network", type: "dhcp"
    node.vm.provider "virtualbox" do |vb|
      vb.customize ["modifyvm", :id, "--nic2", "intnet"]
      vb.customize ["modifyvm", :id, "--intnet2", "intnet2"]
    end

    node.vm.provision "shell", inline: "echo \"Use the project's Makefile to get started\" >> /etc/motd"
  end
  config.vm.define "f" do |node|
    node.vm.box = "generic/fedora28"
    # the intnet configuration makes it dhcp to net-root
    node.vm.network "private_network", type: "dhcp"
    node.vm.provider "virtualbox" do |vb|
      vb.customize ["modifyvm", :id, "--nic2", "intnet"]
      vb.customize ["modifyvm", :id, "--intnet2", "intnet3"]
    end

    node.vm.provision "shell", inline: "echo \"Use the project's Makefile to get started\" >> /etc/motd"
  end
end
