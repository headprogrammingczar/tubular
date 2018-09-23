# VBoxManage options, if all else fails
#   [--nic<1-N> none|null|nat|bridged|intnet|hostonly|
#               generic|natnetwork]
#   [--nictype<1-N> Am79C970A|Am79C973|
#                   82540EM|82543GC|82545EM|
#                   virtio]
#   [--hostonlyadapter<1-N> none|<devicename>]
#   [--intnet<1-N> <network name>]
#   [--cableconnected<1-N> on|off]

# a host-only network above net-root for external access
# requires vboxnet0 host-only network with adapter at ip #{hostonly_subnet}.254
# internal networks simulate wires below
# assume subnets are /24 for testing, strings are easier than math
hostonly_subnet = "192.168.56"
internal_subnet = "192.168.57"

Vagrant.configure("2") do |config|
  config.vm.synced_folder ".", "/vagrant", type: "rsync"

  config.vm.define "net-root" do |node|
    node.vm.box = "generic/openbsd6"

    node.vm.network "private_network", ip: "#{hostonly_subnet}.1"
    # TODO bonding these to the internal subnet's gateway ip is done in bash
    # but vagrant expects addresses :/
    node.vm.network "private_network", ip: "#{internal_subnet}.252"
    node.vm.network "private_network", ip: "#{internal_subnet}.253"

    # vbox-specific settings
    # in vbox each host gets 4 interfaces
    #   one is NAT for vagrant access
    #   one is "up" on the router VM
    #   leaving two for VMs "below" the router
    node.vm.provider "virtualbox" do |vb|
      vb.customize ["modifyvm", :id, "--nic2", "hostonly"]
      vb.customize ["modifyvm", :id, "--nic3", "intnet"]
      vb.customize ["modifyvm", :id, "--nic4", "intnet"]
    end

  end
  config.vm.define "d" do |node|
    node.vm.box = "debian/jessie64"
    node.vm.network "private_network", ip: "#{internal_subnet}.2"
    node.vm.provider "virtualbox" do |vb|
      vb.customize ["modifyvm", :id, "--nic2", "intnet"]
    end
  end
  config.vm.define "f" do |node|
    node.vm.box = "generic/fedora28"
    node.vm.network "private_network", ip: "#{internal_subnet}.3"
    node.vm.provider "virtualbox" do |vb|
      vb.customize ["modifyvm", :id, "--nic2", "intnet"]
    end
  end
end
