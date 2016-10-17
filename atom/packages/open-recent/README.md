# Open Recent Files/Folders

Open recent files in the current window, and recent folders (optionally) in a new window.

## Screenshots

![](http://i.imgur.com/d9y4iAi.png)

You can also open the command palette `Ctrl+Alt+P` and type `open file0`, `open dir0` or `open [filepath]`.

![](http://i.imgur.com/JUed5jx.png)

## Settings

* `maxRecentFiles` and `maxRecentDirectories` limit the number of items in the menu.
* `replaceNewWindowOnOpenDirectory` When true, opening a recent directory will "open" in the current window, but only if the window does not have a project path set. Eg: The window that appears when doing File > New Window.
* `replaceProjectOnOpenDirectory` When true, opening a recent directory will "open" in the current window, replacing the current project.
* `listDirectoriesAddedToProject` When true, the all root directories in a project will be added to the history and not just the 1st root directory.
* `ignoredNames` When true, skips files and directories specified in Atom's "Ignored Names" setting.
