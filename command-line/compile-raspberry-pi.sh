#./configure --enable-single-host
#make from-scratch
export PIPATH=/Volumes/xtools/arm-none-linux-gnueabi
export PATH=$PIPATH/bin:$PATH
export LD=arm-none-linux-gnueabi-ld
export AR=arm-none-linux-gnueabi-ar
export STRIP=arm-none-linux-gnueabi-strip
export RANLIB=arm-none-linux-gnueabi-ranlib
export CC=arm-none-linux-gnueabi-gcc
export CXX=arm-none-linux-gnueabi-g++
export PREFIX=$HOME/gambit-raspberry-pi
#./configure --enable-single-host --prefix=$PREFIX --host=arm-eabi CC=arm-linux-androideabi-gcc CPPFLAGS="-DANDROID -I$ANDROID_NDK_DIR/platforms/android-14/arch-arm/usr/include/ -fno-short-enums" CFLAGS="-DANDROID -fno-short-enums -I$ANDROID_NDK_DIR/platforms/android-14/arch-arm/usr/include/ -nostdlib" LDFLAGS="-Wl,-rpath-link=$ANDROID_NDK_DIR/platforms/android-14/arch-arm/usr/lib/ -L$ANDROID_NDK_DIR/platforms/android-14/arch-arm/usr/lib" LIBS="-lc -ldl"
./configure --host=arm-eabi CFLAGS="-mcpu=cortex-a7 -mfpu=neon-vfpv4 -mfloat-abi=hard" --enable-single-host --enable-c-opt --prefix=$PREFIX
make
make install
