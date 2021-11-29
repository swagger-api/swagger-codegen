# -*- mode: ruby -*-
# vi: set ft=ruby :

# Vagrantfile API/syntax version. Don't touch unless you know what you're doing!
VAGRANTFILE_API_VERSION = "2"

Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|

  config.vm.box = "ubuntu/bionic64"

  config.vm.provider "virtualbox" do |v|
    v.name = "swagger-codegen"
    v.memory = 2048
    v.cpus = 2
  end

  config.vm.box_check_update = true

  #SSH
  config.ssh.forward_agent = true

  config.ssh.shell = "bash -c 'BASH_ENV=/etc/profile exec bash'"

  #Provision
  config.vm.provision "shell", inline: <<-SHELL
    sudo touch /var/lib/cloud/instance/locale-check.skip
    sudo apt-get install ca-certificates curl gnupg lsb-release -y
    curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /usr/share/keyrings/docker-archive-keyring.gpg
    echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/docker-archive-keyring.gpg] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null
    sudo apt-get update
    sudo apt-get upgrade -y
    sudo apt-get install -y docker-ce docker-ce-cli containerd.io
    sudo usermod -aG docker vagrant
  SHELL

end
