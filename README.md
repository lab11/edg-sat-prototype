# Embedded Design Generation Prototype #

This is the prototype repository for Embedded Design Generation, a methodology
that automates much of the embedded design process through judicious use of
existing constraint solvers like Z3.

## Papers and Additional Material ##

### Symposium on Computational Fabrication 2017 ###

Our full appendix for the SCF 2017 paper, including code and details of experiments, can be found [here](https://github.com/lab11/edg-sat-prototype/blob/master/appendix/scf2017.md).


## Dev Environment Setup ## 

A few major options here:

  - **Default:** Works on Win, Mac, and Linux. 
    Creates a VM that's setup to let you build the paper easily. 
  - **LXC:** Works on Linux. 
    Instead of a full VM uses a faster OS virtualisation method. 
    Because the host system is Linux, syscalls in the box can just be
    forwarded to the host OS with some additional permissions checking.
  - **Manual:** Maybe works on Linux? Harder elsewhere. 
    Mostly, you just get to read the `Vagrantfile` and make sure you have
    all the dependencies with their correct versions. 

### Install Vagrant ###

  - *(Default,LXC)* Install [Vagrant](https://www.vagrantup.com) 
      - [Install Instructions](https://www.vagrantup.com/downloads.html)
  - *(LXC)* Install LXC and LXC-Provider
      - Install [LXC](https://linuxcontainers.org/lxc/) 
          - Ubuntu : `sudo apt-get install lxc`
          - Other : [Install Instructions](https://linuxcontainers.org/lxc/getting-started/)
      - Install [Tar](https://www.gnu.org/software/tar/)
          - Ubuntu : `sudo apt-get install tar`
      - Install [Redir](https://linux.die.net/man/1/redir)
          - Ubuntu : `sudo apt-get install redir`
      - Install [Bridge-Utils](https://wiki.linuxfoundation.org/networking/bridge)
          - Ubuntu : `sudo apt-get install brigde-utils`
      - Install [Vagrant-LXC](https://github.com/fgrehm/vagrant-lxc)
          - `vagrant plugin install vagrant-lxc` 

### Setup Build Env ###

  - *(Default,LXC)* Setup a VM/Container
      - Open terminal in project directory. 
      - Tell Vagrant to do all the work:
          - *(Default)* `vagrant up`
          - *(LXC)* `vagrant up --provider=lxc`
      - Enter password if necessary. 
      - Go do something else for a while, the system is installing TexLive.
  - *(Manual)* Setup the Build Env
      - Read the `Vagrantfile`, everything in a `configure.vm.provision` block
         is something you have to replicate. 
         Relevant docs for Vagrant are 
         [here](https://www.vagrantup.com/docs/provisioning/shell.html).
      - List of Dependencies (May be inaccurate):
          - [Git](http://git-scm.com) : Source control
          - TODO : Finish getting full list of dependencies
      - Set everything up yourself. 
          - Good luck, you'll need it.
          - You should read [the Vagrantfile](/Vagrantfile).

## Building ##

  - *(Default,LXC)* Start VM/Container
      - *(Default)* `vagrant up` 
      - *(LXC)* `vagrant up --provider=LXC` 
  - Opening Terminal into Build Environment
      - *(Default,LXC)* `vagrant ssh`
      - *(Manual)* I dunno, you set it up. Figure it out. 
  - TODO : Instructions for building withing the dev env.

## Usage ##

TODO : Add tool use instructions and command line flags.

w/ fish shell
```` 
stack build --executable-profiling --library-profiling --ghc-options="-fprof-auto -rtsopts" ; and stack exec edg-prototype -- simon -o output-data.tmp.edg -g output-graph.tmp.png -s +RTS -p -h

````
