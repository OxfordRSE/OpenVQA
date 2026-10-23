# Developer setup

## Enable checks before committing

After cloning the repository, run these commands from the repository root:

```sh
rv sync
pre-commit install
```

If the `pre-commit` command is unavailable, install it first with
`pipx install pre-commit`.

Run `pre-commit install` once for **each clone**. The tracked configuration
does not enable the Git hook automatically; this command installs it into
`.git/hooks/pre-commit`.

On each commit, the hooks run `styler` and then `lintr` on staged `.R` and `.Rmd`
files. If `styler` changes files, review and stage the changes before retrying
the commit. `lintr` blocks commits containing lines over 100 characters;
`styler` does not automatically wrap every long line, so these may need manual
editing.

To run the checks on staged files without committing, use `pre-commit run`.
