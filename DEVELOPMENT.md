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


# Adding a new version of Prelude

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

5.  Commit&push
