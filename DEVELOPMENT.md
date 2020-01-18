# How to update Command Wrapper Dhall library

1.  Create new branch for this purpose:

    ```
    git checkout -b BRANCH_NAME
    ```

2.  Modify a file in `./dhall/CommandWrapper/` or `./dhall/Exec/` and don't
    forget to update semantic hashes of `package.dhall` files.

3.  Commit your changes and push them to GitHub.

4.  Update commit and appropriate hashes in:

    ```
    src/CommandWrapper/Internal/Subcommand/Completion/DhallExpressions.hs
    ```

    Use hash of the commit created in the step 3.

5.  Rebuild and run tests:

    ```
    stack test
    ```

6.  If all is green then commit&push your changes and do rebase&merge into
    `master`.

    ```
    # We are on our branch at the moment
    git rebase master
    git checkout master
    git git merge --no-ff OUR_BRANCH_NAME
    ```


# Bump Dhall version

1.  Update versions of `dhall`, `dhall-bash`, and `dhall-json` in:

    ```
    stack.yaml
    ```

2.  Update standard version in:

    ```
    src/CommandWrapper/Internal.hs
    ```

3.  Rebuild and run tests:

    ```
    stack test
    ```

4.  Check that output of `version` subcommand is correct by running:

    ```
    stack exec -- command-wrapper version
    ```

5.  With new version of Dhall we usually get new version of Dhall Prelude.  To
    update it follow instructions in section [Adding a new version of Dhall
    Prelude](#adding-a-new-version-of-Dhall-Prelude).

6.  Commit&push


# Adding a new version of Dhall Prelude

1.  Introduce new definitions `preludeV<major>_<minor>_<patch>Import` and
    `preludeV<major>_<minor>_<patch>Content` in:

    ```
    src/CommandWrapper/Internal/Subcommand/Completion/DhallExpressions.hs
    ```

2.  Extend `DhallLibrary` enum in

    ```
    src/CommandWrapper/Internal/Subcommand/Completion/Libraries.hs
    ```

    To include constructor `PreludeV<major>_<minor>_<patch>` and update
    functions `parseDhallLibrary` and `showDhallLibrary` accordingly.

3.  Extend `putDhallLibrary` to handle new cases in `DhallLibrary`.  GHC will
    warn you abot this if the previous step is done.

4.  Introduce a new test case in `test/Main.hs` to make sure that the embedding
    of new Dhall Prelude library behaves correctly.

5.  Rebuild and run tests:

    ```
    stack test
    ```

6.  Update documentation of `--dhall=LIBRARY` option in:

    ```
    man/command-wrapper-completion.1.md
    ```

6.  Commit&push


# Debugging Bash Completion

1.  Lets say that `TOOLSET` variable contains name of our toolset, then we can
    get a completion function for it from currently running bash by calling:

    ```Bash
    type _${TOOLSET}
    ```

    Which will print out the completion function with some extra information on
    top.

2.  Store the function definition returned by `type _${TOOLSET}` into a file.

3.  Modify Bash completion function in the stored file by adding two lines at
    the end:

    ```Bash
    # ... Function definition

        echo "${COMP_CWORD}" "${COMP_WORDS[@]}" >> ~/tmp/completion-debug
        echo "${COMPREPLY[@]}" >> ~/tmp/completion-debug
    }
    ```

4.  Open another terminal to besides the current one so that both can be seen.
    Run following in that new terminal:

    ```Bash
    touch ~/tmp/completion-debug
    tail -f ~/tmp/completion-debug
    ```

5.  Go back to previous terminal and source the file with the modified
    completion function.

6.  Start experimenting with completion and you should be able to see the
    values in the other terminal where the `tail -f` command is running.
