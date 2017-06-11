# nix-derivation-pretty

Pretty printer for Nix derivations built on top of [nix-derivation](https://hackage.haskell.org/package/nix-derivation) library.

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

Requires [Nix](https://nixos.org/nix/) package manager:

```
$ git clone https://github.com/pbogdan/nix-derivation-pretty
$ cd nix-derivation-pretty
$ nix-build
$ nix-env -i ./result
```

## Example (abbreviated) output

### Default

```
Derive ( [("out", "/nix/store/5lvrqrc0nl83d8yih4lgi9510ymv0zp8-dbus-1", "", "")]
       , [ ( "/nix/store/30szc3rl1pajp9bsygnrynnaxxa9k652-NetworkManager-openvpn-gnome-1.2.8.drv"
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
           " )] )
```

### Haskell

```
Derivation
  { outputs =
      fromList
        [ ( "out"
          , DerivationOutput
              { path =
                  FilePath "/nix/store/5lvrqrc0nl83d8yih4lgi9510ymv0zp8-dbus-1"
              , hashAlgo = ""
              , hash = ""
              }
          )
        ]
  , inputDrvs =
      fromList
        [ ( FilePath
              "/nix/store/30szc3rl1pajp9bsygnrynnaxxa9k652-NetworkManager-openvpn-gnome-1.2.8.drv"
          , fromList [ "out" ]
          ) ]
  , inputSrcs =
      fromList
        [ FilePath
            "/nix/store/9krlzvny65gdc8s7kpb6lkx8cd02c25b-default-builder.sh"
        ]
  , platform = "x86_64-linux"
  , builder =
      "/nix/store/wb34dgkpmnssjkq7yj4qbjqxpnapq0lw-bash-4.4-p12/bin/bash"
  , args =
      [ "-e"
      , "/nix/store/9krlzvny65gdc8s7kpb6lkx8cd02c25b-default-builder.sh"
      ]
  , env =
      fromList
        [ ( "XML_CATALOG_FILES"
          , "/nix/store/gxpi0lbnsafydw5iiq77j5vl40jb0903-dbus-catalog.xml"
          )
        , ( "buildCommand"
          , "mkdir -p $out\n\n/nix/store/wd4zp5zjhvbxjk6jsqhkxw9w5aj2v2mr-libxslt-1.1.29-bin/bin/xsltproc --nonet \\\n  --stringparam serviceDirectories \"$serviceDirectories\" \\\n  --stringparam suidHelper \"$suidHelper\" \\\n  /nix/store/lw8gybh5sy95a45ivyn9adzwj7j7fksi-make-system-conf.xsl /nix/store/lgybicd08iv076i9r62gk26w2m98ian0-dbus-1.10.18/share/dbus-1/system.conf \\\n  > $out/system.conf\n/nix/store/wd4zp5zjhvbxjk6jsqhkxw9w5aj2v2mr-libxslt-1.1.29-bin/bin/xsltproc --nonet \\\n  --stringparam serviceDirectories \"$serviceDirectories\" \\\n  /nix/store/fykiafsadp0lcl9arapngjxqwwvagiyd-make-session-conf.xsl /nix/store/lgybicd08iv076i9r62gk26w2m98ian0-dbus-1.10.18/share/dbus-1/session.conf \\\n  > $out/session.conf\n"
          )
        ]
  }
```

### Human

```
/nix/store/9dzk06siq8x4rxb29jjv8wmmvcsrib0c-dbus-1.drv: 
 
  outputs: 
    name: out
      hash: 
      hash algo: 
      input deriviations: 
        "/nix/store/30szc3rl1pajp9bsygnrynnaxxa9k652-NetworkManager-openvpn-gnome-1.2.8.drv"
        "/nix/store/y50xl0lnw9hhg6g3pq36m4bmb3117c32-at-spi2-core-2.22.1.drv" 
  input sources: 
    "/nix/store/9krlzvny65gdc8s7kpb6lkx8cd02c25b-default-builder.sh"
  builder: /nix/store/wb34dgkpmnssjkq7yj4qbjqxpnapq0lw-bash-4.4-p12/bin/bash 
  builder args:  
    -e
    /nix/store/9krlzvny65gdc8s7kpb6lkx8cd02c25b-default-builder.sh 
  environment: 
    XML_CATALOG_FILES: 
      /nix/store/gxpi0lbnsafydw5iiq77j5vl40jb0903-dbus-catalog.xml
    buildCommand: 
      mkdir -p $out
      
      /nix/store/wd4zp5zjhvbxjk6jsqhkxw9w5aj2v2mr-libxslt-1.1.29-bin/bin/xsltproc --nonet \
        --stringparam serviceDirectories "$serviceDirectories" \
        --stringparam suidHelper "$suidHelper" \
        /nix/store/lw8gybh5sy95a45ivyn9adzwj7j7fksi-make-system-conf.xsl /nix/store/lgybicd08iv076i9r62gk26w2m98ian0-dbus-1.10.18/share/dbus-1/system.conf \
        > $out/system.conf
      /nix/store/wd4zp5zjhvbxjk6jsqhkxw9w5aj2v2mr-libxslt-1.1.29-bin/bin/xsltproc --nonet \
        --stringparam serviceDirectories "$serviceDirectories" \
        /nix/store/fykiafsadp0lcl9arapngjxqwwvagiyd-make-session-conf.xsl /nix/store/lgybicd08iv076i9r62gk26w2m98ian0-dbus-1.10.18/share/dbus-1/session.conf \
        > $out/session.conf
```
