# Install Emacs 28.1 with json and native compilation (as recommended for Doom Emacs)

adapted from https://www.taingram.org/blog/emacs-native-comp.html

1. verify gcc version with

    ```shell
    gcc --version
    ```

2. Install libgccjit for the very same version. For example, I did

    ```shell
    sudo apt-get install libgccjit0 libgccjit-11-dev
    ```
3. Proceed with installing emacs

    ```shell
    cd ~/emacs-28.1
    ./autogen.sh
    sudo ./configure --with-json --with-native-compilation
    sudo make -j 8
    sudo make install
    ```
