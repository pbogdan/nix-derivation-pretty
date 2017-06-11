# nix-derivation-pretty

Pretty printer for Nix derivations.

## Usage

```
$ pp-drv ([--haskell] | [--human]) [-w|--width int] file

```

Pretty print a Nix derivation.

Available options:

- `-h,--help`                Show this help text
- `--haskell`                Pretty print haskell data type representing the derivation
- `--human`                  Pretty print more human friendly output
- `-w,--width int`           Column to wrap the output at (default: 80)

## Installation.

Requires [nix](https://nixos.org/nix/) package manager:

```
$ git clone https://github.com/pbogdan/nix-derivation-pretty
$ cd nix-derivation-pretty
$ nix-build
$ nix-env -i -f result
```

## Example output

```
Derive ( [("out", "/nix/store/5lvrqrc0nl83d8yih4lgi9510ymv0zp8-dbus-1", "", "")]
       , [ ( "/nix/store/30szc3rl1pajp9bsygnrynnaxxa9k652-NetworkManager-openvpn-gnome-1.2.8.drv"
           , ["out"] )
         , ( "/nix/store/3cl78xqwiqjqil1kxnmwnr2w8gxrlma1-system-path.drv"
           , ["out"] )
         , ( "/nix/store/45gczn7nlzi2xpy5v118g28bzzf7qq3f-NetworkManager-fortisslvpn-gnome-1.2.4.drv"
           , ["out"] )
         , ( "/nix/store/6363l9pd78p5rzr9858ir3lqzrvab20c-NetworkManager-pptp-gnome-1.2.4.drv"
           , ["out"] )
         , ( "/nix/store/6rjfjbzzwlwwbpmdg06s3h2bclhqn9si-cups-2.2.2.drv"
           , ["out"] )
         , ( "/nix/store/7709sdxwz34rib413pfms7ppkl9mi987-NetworkManager-openconnect-gnome-1.2.4.drv"
           , ["out"] )
         , ( "/nix/store/9l9q1ifa9xqz4jc5xj829xgrg7s4dil1-bash-4.4-p12.drv"
           , ["out"] )
         , ( "/nix/store/9wkdpwcc8dhxdhwf435m45w3v41dlrw3-NetworkManager-l2tp-gnome-1.2.4.drv"
           , ["out"] )
         , ( "/nix/store/aa140dp3wmaf4awmd9sr9nxb7lx65lc9-wpa_supplicant-2.6.drv"
           , ["out"] )
         , ( "/nix/store/an8xr140c10gxvgf4inh2h13mhhb7918-ModemManager-1.6.2.drv"
           , ["out"] )
         , ( "/nix/store/b54nc828wq5slv6945mp8wf3pwlrq6q7-dbus-catalog.xml.drv"
           , ["out"] )
         , ( "/nix/store/c2kvlw7snlq2fzxmvgglsbfxipcxdghw-upower-0.99.4.drv"
           , ["out"] )
         , ( "/nix/store/csmhblcwk8k0h4nr1qyqv8i7z06mvdnv-libxslt-1.1.29.drv"
           , ["bin"] )
         , ( "/nix/store/cw6mp9z332lnr97zdj0n88lvqqxrdfjk-network-manager-1.6.2.drv"
           , ["out"] )
         , ( "/nix/store/h6cvxbwmivcv4lrxkr9zklfsaxnfd3a3-NetworkManager-vpnc-gnome-1.2.4.drv"
           , ["out"] )
         , ( "/nix/store/h9d1blryd7p4786pv3iibdlw9mnp94cr-rtkit-0.11.drv"
           , ["out"] )
         , ("/nix/store/ibczdnhd1h0zdv5mz46i4pybi6m4g79q-stdenv.drv", ["out"])
         , ( "/nix/store/mc09ddsym546r54wvf35wn34r6jjrqp4-udisks-2.1.6.drv"
           , ["out"] )
         , ( "/nix/store/mwgzkz6a7w6d6l8mqkjm6cv73yvqys95-gvfs-1.30.1.drv"
           , ["out"] )
         , ( "/nix/store/rsis36bdz4ymn9z4ilic1fpr8cdcswrb-cups-pk-helper-0.2.6.drv"
           , ["out"] )
         , ( "/nix/store/rsz44lz0y6zimmg4r11jfy5jj1gfm2a3-polkit-0.113.drv"
           , ["out"] )
         , ( "/nix/store/s014qpqf150k90yafgjrv1flk0qlydf5-dbus-1.10.18.drv"
           , ["out"] )
         , ( "/nix/store/wr5k5j8c4g6z0lhh021hg74hqbvmjykw-lightdm-1.22.0.drv"
           , ["out"] )
         , ( "/nix/store/y0p5knbpk0xmqkpgjss03g7v74hm42j1-accountsservice-0.6.43.drv"
           , ["out"] )
         , ( "/nix/store/y50xl0lnw9hhg6g3pq36m4bmb3117c32-at-spi2-core-2.22.1.drv"
           , ["out"] ) ]
       , [ "/nix/store/9krlzvny65gdc8s7kpb6lkx8cd02c25b-default-builder.sh"
         , "/nix/store/fykiafsadp0lcl9arapngjxqwwvagiyd-make-session-conf.xsl"
         , "/nix/store/lw8gybh5sy95a45ivyn9adzwj7j7fksi-make-system-conf.xsl" ]
       , "x86_64-linux"
       , "/nix/store/wb34dgkpmnssjkq7yj4qbjqxpnapq0lw-bash-4.4-p12/bin/bash"
       , [ "-e"
         , "/nix/store/9krlzvny65gdc8s7kpb6lkx8cd02c25b-default-builder.sh" ]
       , [ ( "XML_CATALOG_FILES"
           , "/nix/store/gxpi0lbnsafydw5iiq77j5vl40jb0903-dbus-catalog.xml" )
         , ( "buildCommand"
           , "mkdir -p $out
           
           /nix/store/wd4zp5zjhvbxjk6jsqhkxw9w5aj2v2mr-libxslt-1.1.29-bin/bin/xsltproc --nonet \
             --stringparam serviceDirectories "$serviceDirectories" \
             --stringparam suidHelper "$suidHelper" \
             /nix/store/lw8gybh5sy95a45ivyn9adzwj7j7fksi-make-system-conf.xsl /nix/store/lgybicd08iv076i9r62gk26w2m98ian0-dbus-1.10.18/share/dbus-1/system.conf \
             > $out/system.conf
           /nix/store/wd4zp5zjhvbxjk6jsqhkxw9w5aj2v2mr-libxslt-1.1.29-bin/bin/xsltproc --nonet \
             --stringparam serviceDirectories "$serviceDirectories" \
             /nix/store/fykiafsadp0lcl9arapngjxqwwvagiyd-make-session-conf.xsl /nix/store/lgybicd08iv076i9r62gk26w2m98ian0-dbus-1.10.18/share/dbus-1/session.conf \
             > $out/session.conf
           " )
         , ("buildInputs", "")
         , ( "builder"
           , "/nix/store/wb34dgkpmnssjkq7yj4qbjqxpnapq0lw-bash-4.4-p12/bin/bash" )
         , ("name", "dbus-1")
         , ("nativeBuildInputs", "")
         , ("out", "/nix/store/5lvrqrc0nl83d8yih4lgi9510ymv0zp8-dbus-1")
         , ("passAsFile", "buildCommand")
         , ("propagatedBuildInputs", "")
         , ("propagatedNativeBuildInputs", "")
         , ( "serviceDirectories"
           , "/nix/store/4c2c915yphiasqcr6lbmb6nsphcv7nv7-lightdm-1.22.0 /nix/store/lgybicd08iv076i9r62gk26w2m98ian0-dbus-1.10.18 /nix/store/18l4dnk6qsc5bi5asrhgg8gmiafljqnj-system-path /nix/store/s9hclnp2yzxm1pqjyhmn3fnln6a4nkaw-cups-2.2.2 /nix/store/7s73yy5m5dbayc2lnis9n2wxbxridvs7-cups-pk-helper-0.2.6 /nix/store/h6kqi49cpic7sccrdjzminp50s2p93iy-ModemManager-1.6.2 /nix/store/0d1dq1fjvvx6bk81hxvwqgqdx9l13ljm-network-manager-1.6.2 /nix/store/rd9s5h21l6qpv8r7gyz6vbsc2s2d55f9-NetworkManager-fortisslvpn-gnome-1.2.4 /nix/store/mqwj94jh98442l6gx913mdzl78cm2lyp-NetworkManager-l2tp-gnome-1.2.4 /nix/store/sp7i2lm9d147ysfb9dyqhlz1f232nsai-NetworkManager-openconnect-gnome-1.2.4 /nix/store/gzd01ddnxapfxfh2l6wr1xjdxyjdm71n-NetworkManager-openvpn-gnome-1.2.8 /nix/store/w14fzi4a1wydvk3g6m39fw6yfjksh0zy-NetworkManager-pptp-gnome-1.2.4 /nix/store/3v9gxmfga28361pnzdqv9xwa00dp7zra-NetworkManager-vpnc-gnome-1.2.4 /nix/store/jd5kr87dl0kngw87s7avnpliy97kri64-wpa_supplicant-2.6 /nix/store/rrr6ypcqzgzh2bfm8a51ppvhwrrsgiq8-upower-0.99.4 /nix/store/ac0x9vlg10m89lgwjlcyxg2i0qp0qkxb-udisks-2.1.6 /nix/store/cwpvpz3p9jmm2wayqb16ykwz1wcf7s11-gvfs-1.30.1 /nix/store/71d858yc5m4bhna5x377dp34lz43cw1r-at-spi2-core-2.22.1 /nix/store/7xpk5fp1wzhb035ph54bghxbn5aisjwa-accountsservice-0.6.43 /nix/store/yk77nqwplbngs93nb26agzy1a7hb4zd3-rtkit-0.11 /nix/store/9rnq304q1k1zijb23qdb385kpwqiykik-polkit-0.113" )
         , ("stdenv", "/nix/store/6mk1s81va81dl4jfbhww86cwkl4gyf4j-stdenv")
         , ("suidHelper", "/run/wrappers/bin/dbus-daemon-launch-helper")
         , ("system", "x86_64-linux") ] )
```
