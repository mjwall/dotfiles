minimatch = null

#--- localStorage DB
class DB
  constructor: (@key) ->

  getData: ->
    data = localStorage[@key]
    data = if data? then JSON.parse(data) else {}
    return data

  setData: (data) ->
    localStorage[@key] = JSON.stringify(data)

  removeData: ->
    localStorage.removeItem(@key)

  get: (name) ->
    data = @getData()
    return data[name]

  set: (name, value) ->
    data = @getData()
    data[name] = value
    @setData(data)

  remove: (name) ->
    data = @getData()
    delete data[name]
    @setData(data)


#--- OpenRecent
class OpenRecent
  constructor: ->
    @eventListenerDisposables = []
    @commandListenerDisposables = []
    @localStorageEventListener = @onLocalStorageEvent.bind(@)
    @db = new DB('openRecent')

  #--- Event Handlers
  onUriOpened: ->
    editor = atom.workspace.getActiveTextEditor()
    filePath = editor?.buffer?.file?.path

    # Ignore anything thats not a file.
    return unless filePath
    return unless filePath.indexOf '://' is -1

    @insertFilePath(filePath) if filePath

  onProjectPathChange: (projectPaths) ->
    @insertCurrentPaths()

  onLocalStorageEvent: (e) ->
    if e.key is @db.key
      @update()

  encodeEventName: (s) ->
    s = s.replace('-', '\u2010') # HYPHEN
    s = s.replace(':', '\u02D0') # MO­DI­FI­ER LET­TER TRIANGULAR COLON
    return s

  commandEventName: (prefix, path) ->
    return "open-recent:#{prefix}-#{@encodeEventName(path)}"

  #--- Listeners
  addCommandListeners: ->
    #--- Commands
    # open-recent:File#-path
    for path, index in @db.get('files')
      do (path) => # Explicit closure
        disposable = atom.commands.add "atom-workspace", @commandEventName("File#{index}", path), =>
          @openFile path
        @commandListenerDisposables.push disposable

    # open-recent:Dir#-path
    for path, index in @db.get('paths')
      do (path) => # Explicit closure
        disposable = atom.commands.add "atom-workspace", @commandEventName("Dir#{index}", path), =>
          @openPath path
        @commandListenerDisposables.push disposable

    # open-recent:clear-all------...
    # Add tons of --- at the end to sort this item at the bottom of the command palette.
    # Multiple spaces are ignored inside the command palette.
    disposable = atom.commands.add "atom-workspace", "open-recent:clear-all" + '-'.repeat(1024), =>
      @db.set('files', [])
      @db.set('paths', [])
      @update()
    @commandListenerDisposables.push disposable

  getProjectPath: (path) ->
    return atom.project.getPaths()?[0]

  openFile: (path) ->
    atom.workspace.open path

  openPath: (path) ->
    replaceCurrentProject = false
    options = {}

    if not @getProjectPath() and atom.config.get('open-recent.replaceNewWindowOnOpenDirectory')
      replaceCurrentProject = true
    else if @getProjectPath() and atom.config.get('open-recent.replaceProjectOnOpenDirectory')
      replaceCurrentProject = true

    if replaceCurrentProject
      atom.project.setPaths([path])
      if workspaceElement = atom.views.getView(atom.workspace)
        atom.commands.dispatch workspaceElement, 'tree-view:toggle-focus'
    else
      atom.open {
        pathsToOpen: [path]
        newWindow: !atom.config.get('open-recent.replaceNewWindowOnOpenDirectory')
      }

  addListeners: ->
    #--- Commands
    @addCommandListeners()

    #--- Events
    disposable = atom.workspace.onDidOpen @onUriOpened.bind(@)
    @eventListenerDisposables.push(disposable)

    disposable = atom.project.onDidChangePaths @onProjectPathChange.bind(@)
    @eventListenerDisposables.push(disposable)

    # Notify other windows during a setting data in localStorage.
    window.addEventListener "storage", @localStorageEventListener

  removeCommandListeners: ->
    #--- Commands
    for disposable in @commandListenerDisposables
      disposable.dispose()
    @commandListenerDisposables = []

  removeListeners: ->
    #--- Commands
    @removeCommandListeners()

    #--- Events
    for disposable in @eventListenerDisposables
      disposable.dispose()
    @eventListenerDisposables = []

    window.removeEventListener 'storage', @localStorageEventListener

  #--- Methods
  init: ->
    # Migrate
    if atom.config.get('open-recent.recentDirectories') or atom.config.get('open-recent.recentFiles')
      @db.set('paths', atom.config.get('open-recent.recentDirectories'))
      @db.set('files', atom.config.get('open-recent.recentFiles'))
      atom.config.unset('open-recent.recentDirectories')
      atom.config.unset('open-recent.recentFiles')

    # Defaults
    @db.set('paths', []) unless @db.get('paths')
    @db.set('files', []) unless @db.get('files')

    @addListeners()
    @insertCurrentPaths()
    @update()

  # Returns true if the path should be filtered out, based on settings.
  filterPath: (path) ->
    ignoredNames = atom.config.get('core.ignoredNames')
    if ignoredNames
      minimatch ?= require('minimatch')
      for name in ignoredNames
        match = [name, "**/#{name}/**"].some (comparison) ->
          return minimatch(path, comparison, { matchBase: true, dot: true })
        return true if match

    return false

  insertCurrentPaths: ->
    return unless atom.project.getDirectories().length > 0

    recentPaths = @db.get('paths')
    for projectDirectory, index in atom.project.getDirectories()
      # Ignore the second, third, ... folders in a project
      continue if index > 0 and not atom.config.get('open-recent.listDirectoriesAddedToProject')

      path = projectDirectory.path

      continue if @filterPath(path)

      # Remove if already listed
      index = recentPaths.indexOf path
      if index != -1
        recentPaths.splice index, 1

      recentPaths.splice 0, 0, path

      # Limit
      maxRecentDirectories = atom.config.get('open-recent.maxRecentDirectories')
      if recentPaths.length > maxRecentDirectories
        recentPaths.splice maxRecentDirectories, recentPaths.length - maxRecentDirectories

    @db.set('paths', recentPaths)
    @update()

  insertFilePath: (path) ->
    return if @filterPath(path)

    recentFiles = @db.get('files')

    # Remove if already listed
    index = recentFiles.indexOf path
    if index != -1
      recentFiles.splice index, 1

    recentFiles.splice 0, 0, path

    # Limit
    maxRecentFiles = atom.config.get('open-recent.maxRecentFiles')
    if recentFiles.length > maxRecentFiles
      recentFiles.splice maxRecentFiles, recentFiles.length - maxRecentFiles

    @db.set('files', recentFiles)
    @update()

  #--- Menu
  createSubmenu: ->
    submenu = []
    submenu.push { command: "pane:reopen-closed-item", label: "Reopen Closed File" }
    submenu.push { type: "separator" }

    # Files
    recentFiles = @db.get('files')
    if recentFiles.length
      for path, index in recentFiles
        menuItem = {
          label: path
          command: @commandEventName("File#{index}", path)
        }
        if path.length > 100
          menuItem.label = path.substr(-60)
          menuItem.sublabel = path
        submenu.push menuItem
      submenu.push { type: "separator" }

    # Root Paths
    recentPaths = @db.get('paths')
    if recentPaths.length
      for path, index in recentPaths
        menuItem = {
          label: path
          command: @commandEventName("Dir#{index}", path)
        }
        if path.length > 100
          menuItem.label = path.substr(-60)
          menuItem.sublabel = path
        submenu.push menuItem
      submenu.push { type: "separator" }

    submenu.push { command: "open-recent:clear-all" + '-'.repeat(1024), label: "Clear List" }
    return submenu

  updateMenu: ->
    # Need to place our menu in top section
    for dropdown in atom.menu.template
      if dropdown.label is "File" or dropdown.label is "&File"
        for item in dropdown.submenu
          if item.command is "pane:reopen-closed-item" or item.label is "Open Recent"
            delete item.accelerator
            delete item.command
            delete item.click
            item.label = "Open Recent"
            item.enabled = true
            item.metadata ?= {}
            item.metadata.windowSpecific = false
            item.submenu = @createSubmenu()
            atom.menu.update()
            break # break for item
        break # break for dropdown

  #---
  update: ->
    @removeCommandListeners()
    @updateMenu()
    @addCommandListeners()

  destroy: ->
    @removeListeners()


#--- Module
module.exports =
  config:
    maxRecentFiles:
      type: 'number'
      default: 8
    maxRecentDirectories:
      type: 'number'
      default: 8
    replaceNewWindowOnOpenDirectory:
      type: 'boolean'
      default: true
      description: 'When checked, opening a recent directory will "open" in the current window, but only if the window does not have a project path set. Eg: The window that appears when doing File > New Window.'
    replaceProjectOnOpenDirectory:
      type: 'boolean'
      default: false
      description: 'When checked, opening a recent directory will "open" in the current window, replacing the current project.'
    listDirectoriesAddedToProject:
      type: 'boolean'
      default: false
      description: 'When checked, the all root directories in a project will be added to the history and not just the 1st root directory.'
    ignoredNames:
      type: 'boolean'
      default: true
      description: 'When checked, skips files and directories specified in Atom\'s "Ignored Names" setting.'

  instance: null

  activate: ->
    @instance = new OpenRecent()
    @instance.init()

  deactivate: ->
    @instance.destroy()
    @instance = null
