---
title: "Install JDK on macOS"
tags: java
classes: wide
published: true
---

This article will describe how to install the Java Development Kit (JDK) on macOS. Supporting multiple versions and switching between them will also be covered.

## Download and Install

Go to [AdoptOpenJDK](https://adoptopenjdk.net/?variant=openjdk11&jvmVariant=hotspot). I prefer these prebuilt OpenJDK binaries as they are maintained with fixes to the JDK.

Download the release you need. I currently use the 11 LTS release. Now run the installer.

## Where Is It?

The installer installs the JDK to `/Library/Java/JavaVirtualMachines/adoptopenjdk-11.jdk`.

Run the `java_home` command to print the matching list of JVMs and architectures:

```bash
$ /usr/libexec/java_home -V
Matching Java Virtual Machines (2):
    11.0.6, x86_64:	"AdoptOpenJDK 11"	/Library/Java/JavaVirtualMachines/adoptopenjdk-11.jdk/Contents/Home
    1.8.0_232, x86_64:	"AdoptOpenJDK 8"	/Library/Java/JavaVirtualMachines/adoptopenjdk-8.jdk/Contents/Home

/Library/Java/JavaVirtualMachines/adoptopenjdk-11.jdk/Contents/Home
```

## Test It

Run `java -version` in a shell to see the version and vendor of the JDK.

```bash
$ java --version
openjdk 11.0.6 2020-01-14
OpenJDK Runtime Environment AdoptOpenJDK (build 11.0.6+10)
OpenJDK 64-Bit Server VM AdoptOpenJDK (build 11.0.6+10, mixed mode)
```

Create a quick "hello world" Java source file to test the compiler:

```bash
$ cat > HelloWorld.java
class HelloWorld {
  public static void main(String[] args) {
    System.out.println("hello world");
  }
}

^Z
[1]+  Stopped                 cat > HelloWorld.java
```

Redirecting `cat` to a filename is a quick way to enter a small amount of text without leaving the shell. Press `Ctrl+Z` to quit editing and to save the text to the file.

Now run the file:

```bash
$ java HelloWorld.java
hello world
```

Note that newer versions of the JDK can compile and run with a single command as this example has shown.

## JShell

Did you know that Java now has a REPL (read-eval-print-loop) shell? Run `jshell` in a terminal shell. Then write a line of "hello world" code.

```bash
$ jshell
|  Welcome to JShell -- Version 11.0.6
|  For an introduction type: /help intro

jshell> System.out.println("hello world");
hello world
```

Use `/exit` to exit jshell.

```bash
jshell> /exit
|  Goodbye
```

## JAVA_HOME

In this section we are going to configure the shell environment of macOS. `JAVA_HOME` is an environment variable set in the operating system that is a convention that many libraries and tools in the Java ecosystem need. It defines the home path for the installed JDK.

Put the following text into the file `.bash_profile` in your user directory (`~` or `/Users/<username>`):

```bash
if [ -f ~/.bashrc ]; then
  source ~/.bashrc
fi
```

The .bash_profile is executed for login shells. The contents of this file sources the `.bashrc` file if it exists upon login.

Define the `JAVA_HOME` environment variable in the `.bashrc` file in your user directory as follows:

```bash
export JAVA_HOME=/usr/libexec/java_home
export PATH="$JAVA_HOME/bin:/usr/local/bin:$PATH"
```

The `.bashrc` file executes when an interactive shell is started. Set `JAVA_HOME` to `/usr/libexec/java_home`. This is a utility specific to macOS. Notice that `$JAVA_HOME/bin` is prepended to the `PATH` environment variable.

## Switch Versions

Now that the environment variables are set we can manually switch between JDK versions.

For version 11 append `-v 11` to the JAVA_HOME environment variable in the `.bashrc` file:

```bash
export JAVA_HOME=`/usr/libexec/java_home -v 11`
```

For version 8:

```bash
export JAVA_HOME=`/usr/libexec/java_home -v 1.8`
```

After updating the `.bashrc` file you'll need to source it:

```bash
$ source .bashrc
```

To see what version of the JDK is being used run the following command:

```bash
$ echo $JAVA_HOME
/Library/Java/JavaVirtualMachines/adoptopenjdk-11.jdk/Contents/Home
```

## Summary

This article describes a simple way to install the JDK with some environment configuration.

## References

[AdoptOpenJDK](https://adoptopenjdk.net/)  
[java_home and JAVA_HOME on macOS](https://medium.com/notes-for-geeks/java-home-and-java-home-on-macos-f246cab643bd)
