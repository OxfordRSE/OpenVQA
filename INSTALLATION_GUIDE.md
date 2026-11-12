# Install OpenVQA on macOS or Windows

> [!WARNING]
> **Supported: macOS with R 4.5.0 or R 4.5.2. Windows support is preliminary, using R 4.5.2 on Intel/AMD 64-bit PCs.** All six demo assessments run successfully in Windows CI, but strict numerical comparisons still fail for PCS, CH and BAES. Windows figure parity is not checked. Other R versions and Linux are outside this guide. Use R 4.5.2 for a new installation.

You need an internet connection; you do not need to know R or install RStudio.

## 1. Open a command window

- **macOS:** press **Command + Space**, type **Terminal**, and press **Return**.
- **Windows:** open **Start**, type **PowerShell**, and open **Windows PowerShell**.

These apps let you run commands by typing them. Copy one command at a time, press **Return/Enter**, and wait for the prompt to return before continuing. Follow the instructions for your operating system.

Run these commands here, not in the R application. If asked for your Mac password, typing will not show any characters; type it and press Return.

## 2. Install R

Choose **one** of these routes. Skip this step if you already have a supported R version.

**Option A — standard installer (simplest).**

On **macOS**, open **Apple menu → About This Mac** to check your chip/processor. Download the matching R 4.5.2 installer:

- [Apple silicon (M1, M2, M3, etc.)](https://cran.r-project.org/bin/macosx/big-sur-arm64/base/R-4.5.2-arm64.pkg)
- [Intel](https://cran.r-project.org/bin/macosx/big-sur-x86_64/base/R-4.5.2-x86_64.pkg)

Open the downloaded `.pkg` file and follow the installer. Close and reopen Terminal.

On **Windows**, download [R 4.5.2 for Windows](https://cran.r-project.org/bin/windows/base/old/4.5.2/R-4.5.2-win.exe), open the `.exe` and follow the installer. In File Explorer, find the installed folder containing `Rscript.exe`, usually `C:\Program Files\R\R-4.5.2\bin\x64`, and add it to your **Path** as follows:

1. Open Start and search for **Edit environment variables for your account**.
2. Under **User variables**, select **Path → Edit → New** and paste the folder path. Keep existing entries.
3. Click **OK** to close each dialog, then close and reopen PowerShell.

This lets PowerShell find installed commands. Use the same steps when adding rv below.

**Option B — rig (useful for managing R versions).** On macOS, use the installer linked in the [official rig instructions](https://rig.r-lib.org/install.html). On Windows, install rig using those instructions or run:

```powershell
winget install posit.rig
```

Reopen your command window, then run:

```sh
rig add 4.5.2
rig list
```

Find R 4.5.2 in the list and use its **name** to select it. For example, if its name is `4.5-arm64`, run:

```sh
rig default 4.5-arm64
```

Use the name shown on your machine if it differs.

With either route, check which R your command window will use:

```sh
Rscript --version
```

It must report **4.5.2** (macOS also supports **4.5.0**). Stop here if it reports another version; with rig, select the supported version's name as above. Avoid automatic R upgrades while using OpenVQA.

## 3. Install rv

`rv` downloads the R packages OpenVQA needs. On **macOS**, run the
[official installer](https://a2-ai.github.io/rv-docs/intro/installation/):

```sh
curl -fsSL https://raw.githubusercontent.com/A2-ai/rv/refs/heads/main/scripts/install.sh | bash
```

Close and reopen Terminal so it can find the installed command, then check:

```sh
rv --version
```

It should print an rv version number. If you already use Homebrew, you can install rv with `brew install rv-r` instead of the installer above.

On **Windows**, download the ZIP ending in `x86_64-pc-windows-msvc.zip` from the [rv 0.22.2 release](https://github.com/A2-ai/rv/releases/tag/v0.22.2) (the version used in CI). Right-click it and choose **Extract All**. Move the folder containing `rv.exe` somewhere permanent, add that folder to your **Path** using step 2 above, then reopen PowerShell and run:

```powershell
rv.exe --version
```

Always write **`rv.exe`** in PowerShell: `rv` is also the name of an unrelated built-in command.

## 4. Download OpenVQA and open its folder

1. Open [OpenVQA on GitHub](https://github.com/OxfordRSE/OpenVQA), then choose
   **Code → Download ZIP**. No GitHub account or Git installation is needed.
2. Extract the ZIP: double-click it in Finder on macOS; use **Extract All** in File Explorer on Windows. Keep the whole extracted folder.
3. Select that folder in your command window:
   - **macOS:** type `cd` followed by a space, drag the extracted folder from Finder into Terminal, then press Return.
   - **Windows:** copy the extracted folder's path from File Explorer's address bar, then run `cd "C:\path\to\OpenVQA-main"`, replacing the example path with yours. Keep the quotes.
4. Run `ls` on macOS or `dir` on Windows. The listing should include `analyse.R` and `rproject.toml`.

Repeat step 3 whenever you open a new command window to use OpenVQA.

## 5. Install dependencies and check OpenVQA

From the OpenVQA folder, run **one** command for your system. On macOS:

```sh
rv sync
```

On Windows, use this instead:

```powershell
rv.exe sync
```

The first run may take several minutes. Wait for the prompt to return and check there is no installation error. If Windows reports missing build tools when compiling packages, install [Rtools45](https://cran.r-project.org/bin/windows/Rtools/rtools45/rtools.html) with its defaults, reopen PowerShell and retry `rv.exe sync`. OpenVQA selects its project package library automatically; no separate activation command is needed.

Finally, print the OpenVQA software version:

```sh
Rscript analyse.R --version
```

For version 0.1.0, the output is `OpenVQA 0.1.0`.

## 6. Run a demo

These commands work in both Terminal and PowerShell. First check the supplied demo inputs, then run the analysis:

```sh
Rscript validate.R --data-root data/vqa-demo2-min
Rscript analyse.R --data-root data/vqa-demo2-min --assessment project_current --seed 20260810
```

Wait for the analysis to finish. In Finder or File Explorer, open `data/vqa-demo2-min/project_current/`: tables and `vqa_summary.xlsx` are in **results**, figures in **figs**, and logs in **log**. The project's settings are in `data/vqa-demo2-min/params.yaml`; assessment inputs are in `project_current/inputs/`. Rerunning an assessment can replace its generated outputs. Add `--no-figures` to the analysis command to try numerical output alone, as in Windows CI.

See [Run an assessment](README.md#run-an-assessment) for the other demo and [Configuration contract](README.md#configuration-contract) for settings.

If a command fails, stop and keep its error message. `command not found` or `not recognized` usually means the tool is not installed, its folder is missing from Path, or the command window needs reopening. An error about `analyse.R` not being found means you need to select the OpenVQA folder again.
