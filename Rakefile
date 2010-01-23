#
#
#

require 'rake/clean'

task :check => [:clean] do
	system 'R CMD check .'
end

task :build => [:clean] do
	system 'R CMD build .'
end

CLEAN.include('*.tar.gz')
