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
# internal networks simulate wires below
hostonly_net = "vboxnet0"

# assumes /24 for testing
hostonly_subnet = "192.168.56"
internal_subnet = "192.168.57"

Vagrant.configure("2") do |config|
  config.vm.synced_folder ".", "/vagrant", type: "rsync"

  # for the boxes
  config.vm.provider "virtualbox" do |vb|
    #vb.customize ["modifyvm", :id, "--cpuexecutioncap", "50"]
  end

  config.vm.define "net-root" do |node|
    node.vm.box = "generic/openbsd6"

    # TODO
    node.vm.network "private_network", ip: "#{hostonly_subnet}.1"
    node.vm.network "private_network", ip: "#{internal_subnet}.1"
    node.vm.network "private_network", ip: "#{internal_subnet}.2"
    node.vm.network "private_network", ip: "#{internal_subnet}.3"
    node.vm.network "private_network", ip: "#{internal_subnet}.4"
  end
  config.vm.define "d1" do |node|
    node.vm.box = "debian/jessie64"
  end
  config.vm.define "f1" do |node|
    node.vm.box = "generic/fedora28"
  end
  config.vm.define "d2" do |node|
    node.vm.box = "debian/jessie64"
  end
  config.vm.define "f2" do |node|
    node.vm.box = "generic/fedora28"
  end
end
