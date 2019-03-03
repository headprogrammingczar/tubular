# each vm has a nat interface automatically that should only be used for "vagrant ssh"
# a host-only network above net-root for external access
# requires vboxnet0 host-only network with adapter at ip #{hostonly_subnet}.254
# internal networks simulate wires below
# assume subnets are /24 for testing, strings are easier than math
hostonly_subnet = "192.168.56"
internal_subnet = "192.168.57"

def envint(s)
  Integer(ENV[s])
rescue TypeError
  nil
rescue ArgumentError
  nil
end

# build_mem must be at least 1024 per cpu
# 4 cpus seems to be ideal for build time
build_mem = envint('BUILDMEM') || 4096
build_cpus = envint('BUILDCPUS') || 4

Vagrant.configure("2") do |config|
  config.vm.define "build" do |node|
    node.vm.box = "generic/freebsd11"
    node.vm.provision "shell", inline: <<-EOF
      curl -sSL https://get.haskellstack.org/ | sh
      echo 'REFUSE accessibility arabic archivers astro audio benchmarks biology cad chinese comms converters databases deskutils devel dns editors emulators finance french ftp games german graphics hebrew hungarian irc japanese java korean lang mail math misc multimedia net news palm polish ports-mgmt portuguese print russian science security shells sysutils textproc ukrainian vietnamese www x11' >> /etc/portsnap.conf
      # lie to portsnap because it can damn well run non-interactively
      # and portsnap cron waits up to an hour
      portsnap fetch extract --interactive
      echo "Use the project's Makefile to get started" >> /etc/motd
    EOF

    node.vm.provider "virtualbox" do |v|
      v.memory = build_mem
      v.cpus = build_cpus
    end
  end

  config.vm.define "net-root" do |node|
    node.vm.box = "generic/freebsd11"

    # let virtualbox be the parent network's gateway, since it doesn't matter and is easy
    node.vm.network "private_network", type: "dhcp"
    # bonding these to the internal subnet's gateway ip is done in bash
    # but vagrant expects addresses :/
    node.vm.network "private_network", ip: "#{internal_subnet}.253"
    node.vm.network "private_network", ip: "#{internal_subnet}.254"

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
    node.vm.provision "shell", inline: <<-EOF
      echo "Use the project's Makefile to get started
or run sudo service tubulard onestart" >> /etc/motd
    EOF
  end

  config.vm.define "d" do |node|
    node.vm.box = "debian/jessie64"
    # the intnet configuration makes it dhcp to net-root
    node.vm.network "private_network", type: "dhcp"
    node.vm.provider "virtualbox" do |vb|
      vb.customize ["modifyvm", :id, "--nic2", "intnet"]
      vb.customize ["modifyvm", :id, "--intnet2", "intnet2"]
    end
    node.vm.provision "shell", inline: <<-EOF
      echo "Use the project's Makefile to get started" >> /etc/motd
    EOF
  end

  config.vm.define "f" do |node|
    node.vm.box = "generic/fedora29"
    # the intnet configuration makes it dhcp to net-root
    node.vm.network "private_network", type: "dhcp"
    node.vm.provider "virtualbox" do |vb|
      vb.customize ["modifyvm", :id, "--nic2", "intnet"]
      vb.customize ["modifyvm", :id, "--intnet2", "intnet3"]
    end
    node.vm.provision "shell", inline: <<-EOF
      echo "Use the project's Makefile to get started" >> /etc/motd
    EOF
  end
end
