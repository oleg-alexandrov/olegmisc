#!/System/Library/Frameworks/Ruby.framework/Versions/Current/usr/bin/ruby

# SET YOUR_HOME TO THE ABSOLUTE PATH OF YOUR HOME DIRECTORY
# chmod +x install.rb
# ./install.rb
YOUR_HOME = ''

HOMEBREW_PREFIX = "/Users/oalexan1/usr/local"
HOMEBREW_CACHE = '/Library/Caches/Homebrew'
HOMEBREW_REPO = 'https://github.com/Homebrew/brew'

module Tty extend self
  def blue; bold 34; end
  def white; bold 39; end
  def red; underline 31; end
  def reset; escape 0; end
  def bold n; escape "1;#{n}" end
  def underline n; escape "4;#{n}" end
  def escape n; "\033[#{n}m" if STDOUT.tty? end
end

class Array
  def shell_s
    cp = dup
    first = cp.shift
    cp.map{ |arg| arg.gsub " ", "\\ " }.unshift(first) * " "
  end
end

def ohai *args
  puts "#{Tty.blue}==>#{Tty.white} #{args.shell_s}#{Tty.reset}"
end

def warn warning
  puts "#{Tty.red}Warning#{Tty.reset}: #{warning.chomp}"
end

def system *args
  abort "Failed during: #{args.shell_s}" unless Kernel.system(*args)
end

def sudo *args
  #ohai "/usr/bin/sudo", *args
  #system "/usr/bin/sudo", *args
  system *args
end

def getc  # NOTE only tested on OS X
  system "/bin/stty raw -echo"
  if STDIN.respond_to?(:getbyte)
    STDIN.getbyte
  else
    STDIN.getc
  end
ensure
  system "/bin/stty -raw echo"
end

def wait_for_user
  puts
  puts "Press RETURN to continue or any other key to abort"
  c = getc
  # we test for \r and \n because some stuff does \r instead
  abort unless c == 13 or c == 10
end

module Version
  def <=>(other)
    split(".").map { |i| i.to_i } <=> other.split(".").map { |i| i.to_i }
  end
end

def macos_version
  @macos_version ||= `/usr/bin/sw_vers -productVersion`.chomp[/10\.\d+/].extend(Version)
end

def git
  @git ||= if ENV['GIT'] and File.executable? ENV['GIT']
    ENV['GIT']
  elsif Kernel.system '/usr/bin/which -s git'
    'git'
  else
    exe = `xcrun -find git 2>/dev/null`.chomp
    exe if $? && $?.success? && !exe.empty? && File.executable?(exe)
  end

  return unless @git
  # Github only supports HTTPS fetches on 1.7.10 or later:
  # https://help.github.com/articles/https-cloning-errors
  `#{@git} --version` =~ /git version (\d\.\d+\.\d+)/
  return if $1.nil? or $1.extend(Version) < "1.7.10"

  @git
end

def chmod?(d)
  File.directory?(d) && !(File.readable?(d) && File.writable?(d) && File.executable?(d))
end

def chgrp?(d)
  !File.grpowned?(d)
end

# Invalidate sudo timestamp before exiting
#at_exit { Kernel.system "/usr/bin/sudo", "-k" }

# The block form of Dir.chdir fails later if Dir.CWD doesn't exist which I
# guess is fair enough. Also sudo prints a warning message for no good reason
Dir.chdir "#{YOUR_HOME}/usr"

####################################################################### script
abort "See Linuxbrew: http://brew.sh/linuxbrew/" if /linux/i === RUBY_PLATFORM
abort "MacOS too old, see: https://github.com/mistydemeo/tigerbrew" if macos_version < "10.5"
abort "Don't run this as root!" if Process.uid == 0
#abort <<-EOABORT unless `groups`.split.include? "admin"
#This script requires the user #{ENV['USER']} to be an Administrator. If this
#sucks for you then you can install Homebrew in your home directory or however
#you please; please refer to our homepage. If you still want to use this script
#set your user to be an Administrator in System Preferences or `su' to a
#non-root user with Administrator privileges.
#EOABORT
abort <<-EOABORT unless Dir["#{HOMEBREW_PREFIX}/.git/*"].empty?
It appears Homebrew is already installed. If your intent is to reinstall you
should do the following before running this installer again:
    rm -rf #{HOMEBREW_PREFIX}/Cellar #{HOMEBREW_PREFIX}/.git && brew cleanup
EOABORT
# Tests will fail if the prefix exists, but we don't have execution
# permissions. Abort in this case.
abort <<-EOABORT if File.directory? HOMEBREW_PREFIX and not File.executable? HOMEBREW_PREFIX
The Homebrew prefix, #{HOMEBREW_PREFIX}, exists but is not searchable. If this is
not intentional, please restore the default permissions and try running the
installer again:
    sudo chmod 775 #{HOMEBREW_PREFIX}
EOABORT
abort <<-EOABORT if `/usr/bin/xcrun clang 2>&1` =~ /license/ && !$?.success?
You have not agreed to the Xcode license.
Before running the installer again please agree to the license by opening
Xcode.app or running:
    sudo xcodebuild -license
EOABORT

ohai "This script will install:"
puts "#{HOMEBREW_PREFIX}/bin/brew"
puts "#{HOMEBREW_PREFIX}/Library/..."
puts "#{HOMEBREW_PREFIX}/share/man/man1/brew.1"

chmods = %w( . bin etc include lib lib/pkgconfig Library sbin share var var/log share/locale share/man
             share/man/man1 share/man/man2 share/man/man3 share/man/man4
             share/man/man5 share/man/man6 share/man/man7 share/man/man8
             share/info share/doc share/aclocal ).
             map { |d| File.join(HOMEBREW_PREFIX, d) }.select { |d| chmod?(d) }
chgrps = chmods.select { |d| chgrp?(d) }

unless chmods.empty?
  ohai "The following directories will be made group writable:"
  puts(*chmods)
end
unless chgrps.empty?
  ohai "The following directories will have their group set to #{Tty.underline 39}admin#{Tty.reset}:"
  puts(*chgrps)
end

wait_for_user if STDIN.tty?

if File.directory? HOMEBREW_PREFIX
  sudo "/bin/chmod", "g+rwx", *chmods unless chmods.empty?
  sudo "/usr/bin/chgrp", "admin", *chgrps unless chgrps.empty?
else
  sudo "/bin/mkdir", HOMEBREW_PREFIX
  sudo "/bin/chmod", "g+rwx", HOMEBREW_PREFIX
  # the group is set to wheel by default for some reason
  sudo "/usr/bin/chgrp", "admin", HOMEBREW_PREFIX
end

sudo "/bin/mkdir", HOMEBREW_CACHE unless File.directory? HOMEBREW_CACHE
sudo "/bin/chmod", "g+rwx", HOMEBREW_CACHE if chmod? HOMEBREW_CACHE

if macos_version >= "10.9"
  developer_dir = `/usr/bin/xcode-select -print-path 2>/dev/null`.chomp
  if developer_dir.empty? || !File.exist?("#{developer_dir}/usr/bin/git")
    ohai "Installing the Command Line Tools (expect a GUI popup):"
    sudo "/usr/bin/xcode-select", "--install"
    puts "Press any key when the installation has completed."
    getc
  end
end

ohai "Downloading and installing Homebrew..."
Dir.chdir HOMEBREW_PREFIX do
  if git
    # we do it in four steps to avoid merge errors when reinstalling
    system git, "init", "-q"

    # "git remote add" will fail if the remote is defined in the global config
    system git, "config", "remote.origin.url", HOMEBREW_REPO
    system git, "config", "remote.origin.fetch", "+refs/heads/*:refs/remotes/origin/*"

    args = git, "fetch", "origin", "master:refs/remotes/origin/master", "-n"
    args << "--depth=1" if ARGV.include? "--fast"
    system(*args)

    system git, "reset", "--hard", "origin/master"
  else
    # -m to stop tar erroring out if it can't modify the mtime for root owned directories
    # pipefail to cause the exit status from curl to propogate if it fails
    # we use -k for curl because Leopard has a bunch of bad SSL certificates
    curl_flags = "fsSL"
    curl_flags << "k" if macos_version <= "10.5"
    system "/bin/bash -o pipefail -c '/usr/bin/curl -#{curl_flags} #{HOMEBREW_REPO}/tarball/master | /usr/bin/tar xz -m --strip 1'"
  end
end

warn "#{HOMEBREW_PREFIX}/bin is not in your PATH." unless ENV['PATH'].split(':').include? "#{HOMEBREW_PREFIX}/bin"

ohai "Installation successful!"
ohai "Next steps"

if macos_version < "10.9" and macos_version > "10.6"
  `/usr/bin/cc --version 2> /dev/null` =~ %r[clang-(\d{2,})]
  version = $1.to_i
  puts "Install the #{Tty.white}Command Line Tools for Xcode#{Tty.reset}: https://developer.apple.com/downloads" if version < 425
else
  puts "Install #{Tty.white}Xcode#{Tty.reset}: https://developer.apple.com/xcode" unless File.exist? "/usr/bin/cc"
end

puts "Run `brew doctor` #{Tty.white}before#{Tty.reset} you install anything"
puts "Run `brew help` to get started"
