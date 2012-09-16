# This is a script to take a blank debian6 virtual machine to a roguestar server.  It runs as root and sets up a roguestar that must also run as root, which for many people will be unacceptable.

# It is a work in progress.  In other words, it does not work.

aptitude update &&
aptitude install make bzip2 ufw git zlib1g-dev libgl1-mesa-dev libglc-dev freeglut3-dev libedit-dev libglw1-mesa-dev uuid-dev &&
ufw default deny &&
ufw allow ssh/tcp &&
ufw allow http/tcp &&
ufw enable &&

wget http://www.haskell.org/ghc/dist/7.4.1/ghc-7.4.1-i386-unknown-linux.tar.bz2 &&
tar xjvf ghc-7.4.1-i386-unknown-linux.tar.bz2 &&
cd ghc-7.4.1 &&
./configure &&
make install &&
cd

wget http://lambda.haskell.org/platform/download/2012.2.0.0/haskell-platform-2012.2.0.0.tar.gz &&
tar xzvf haskell-platform-2012.2.0.0.tar.gz &&

cd haskell-platform-2012.2.0.0/ &&
./configure &&
make &&
make install &&
cd &&

cabal update &&

cabal install angel &&

# everything after this will fail miserably due to missing cabal depends

git clone https://github.com/clanehin/roguestar.git &&
cd roguestar &&
cabal configure &&
cabal build

