if type -q yum
  alias update "sudo yum -y update && yum clean all"
end
if type -q apt
  alias update "sudo apt update && sudo apt -y upgrade && sudo apt -y dist-upgrade && sudo apt -y autoremove && sudo apt -y autoclean"
end
