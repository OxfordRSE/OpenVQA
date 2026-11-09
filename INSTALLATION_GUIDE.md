# Install OpenVQA on macOS

**Currently supported: macOS with R 4.5.0 or R 4.5.2 only.** Use R 4.5.2 for a new
installation. Other R versions, Windows and Linux are not currently supported.
You need an internet connection; you do not need to know R or install RStudio.

## 1. Open Terminal

Press **Command + Space**, type **Terminal**, and press **Return**. This app lets you run commands by typing them.

For each command in the sections below::

1. copy one line into Terminal,
2. press Return,
3. wait for it to finish before continuing.

Run these commands in Terminal, not in the R application. If asked for your Mac password, typing will not show any characters; type it and press Return.

## 2. Install R

Choose **one** of these routes. Skip this step if you already have a supported R version.

**Option A — standard installer (simplest).** Open **Apple menu → About This Mac** to check your chip/processor. Download the matching R 4.5.2 installer:

- [Apple silicon (M1, M2, M3, etc.)](https://cran.r-project.org/bin/macosx/big-sur-arm64/base/R-4.5.2-arm64.pkg)
- [Intel](https://cran.r-project.org/bin/macosx/big-sur-x86_64/base/R-4.5.2-x86_64.pkg)

Open the downloaded `.pkg` file and follow the installer. Close and reopen Terminal.

**Option B — rig (useful for managing R versions).** Install rig using the macOS installer linked in the [official rig instructions](https://rig.r-lib.org/install.html). Reopen Terminal, then run:

```sh
rig add 4.5.2
rig list
```

Find R 4.5.2 in the list and use its **name** to select it. For example, if its name is `4.5-arm64`, run:

```sh
rig default 4.5-arm64
```

Use the name shown on your machine if it differs.

With either route, check which R your Terminal will use:

```sh
Rscript --version
```

It must report **4.5.2** or **4.5.0**. Stop here if it reports another version; with rig, select the supported version's name as above. Avoid automatic R upgrades while using OpenVQA.

## 3. Install rv

`rv` downloads the R packages OpenVQA needs. Run the
[official installer](https://a2-ai.github.io/rv-docs/intro/installation/):

```sh
curl -fsSL https://raw.githubusercontent.com/A2-ai/rv/refs/heads/main/scripts/install.sh | bash
```

Close and reopen Terminal so it can find the installed command, then check:

```sh
rv --version
```

It should print an rv version number. If you already use Homebrew, you can install rv with `brew install rv-r` instead of the installer above.

## 4. Download OpenVQA and open its folder

1. Open [OpenVQA on GitHub](https://github.com/OxfordRSE/OpenVQA), then choose
   **Code → Download ZIP**. No GitHub account or Git installation is needed.
2. Double-click the ZIP in Finder to extract it. Keep the whole extracted folder.
3. In Terminal, type `cd` followed by a **space**, drag the extracted folder from
   Finder into the Terminal window, then press Return. This selects that folder as the place where commands run.
4. Run `ls`. The listing should include `analyse.R` and `rproject.toml`.

Repeat step 3 whenever you open a new Terminal window to use OpenVQA.

## 5. Install dependencies and check OpenVQA

From the OpenVQA folder, run:

```sh
rv sync
```

The first run may take several minutes. Wait for Terminal's prompt to return and check there is no installation error. OpenVQA selects its project package library automatically; no separate activation command is needed.

Finally, print the OpenVQA software version:

```sh
Rscript analyse.R --version
```

For version 0.1.0, the output is `OpenVQA 0.1.0`. Installation is now complete; continue with [Run an assessment](README.md#run-an-assessment) to try the demos.

If a command fails, stop and keep its error message. `command not found` usually means the tool is not installed or Terminal needs reopening; an error about `analyse.R` not being found means you need to select the OpenVQA folder again.
