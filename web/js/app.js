$(document).ready(function() {
  new Vue({
    el: '#top-div',
    data: () => {
      return {
        todos: [],
        todoSearch: "",
        loading: true,
        priorityFilter: "any",
        sortMultiplier: {'priority': 1},
        customSortSelected: '',
        customAttributeKeys: [],
        setAssignee: "",
        addTags: "",
        setPriority: null,
        lastSortField: null,
        addKeyVals: "",
        addKeyValParseError: false,
        nothingFilledError: false,
        license: null,
        loadingLicense: false,
        limited: false,
      }
    },
    created: function() {
      this.refresh()()
      return this.getLicense()
    },
    methods: {
      // higher order for ease of calling from vue templates
      refresh: function(recompute) {
        return function() {
          this.loading = true
          $.ajax("/todos?recompute=" + !!recompute, {
            type: "GET",
            dataType: "json",
            success: function(data) {
              this.todos = data.todos.map(t => {
                return {
                  id: t.entryId,
                  body: t.body.join("\n"),
                  assignee: t.assignee,
                  sourceFile: t.sourceFile,
                  lineNumber: t.lineNumber,
                  priority: t.priority,
                  flag: t.flag,
                  customAttributes: t.customAttributes.reduce((acc, curr) => {
                    console.log(acc, curr)
                    acc[curr[0]] = curr[1]
                    console.log(acc, curr)
                    return acc
                  }, {}),
                  tags: t.tags,
                  selected: false
                }
              })
              this.limited = data.limited
              this.loading = false

              if (!recompute) {
                this.sortTodos('priority')
              } else {
                const field = this.lastSortField || 'priority'
                this.sortMultiplier[field] *= -1 // reset the multipler
                this.sortTodos(field)
              }

              this.customAttributeKeys = Array.from(new Set([].concat.apply([], this.todos.map(t => {
                return Object.keys(t.customAttributes)
              }))))

              this.customAttributeKeys.sort()
              this.hideDropdown()
            }.bind(this),
            error: function() {
              this.loading = false
              alert("Error! D: Check your connection to the server")
            }.bind(this)
          })
        }.bind(this)
      },

      sortTodos: function(sortField) {
        this.lastSortField = sortField
        if (!sortField || typeof sortField !== 'string') {
          sortField = this.customSortSelected
        }
        // multiplier for custom sort val that does asc / desc sort
        const multiplier = this.sortMultiplier[sortField] || 1
        if (sortField === 'priority') {
          // so we keep nulls at the end always when sorting
          const valDefault = multiplier > 0 ? Number.MAX_SAFE_INTEGER : Number.MIN_SAFE_INTEGER
          this.todos.sort(function(a, b) {
            const _a = (a.priority !== null ? a.priority : valDefault)
            const _b = (b.priority !== null ? b.priority : valDefault)
            if (_a < _b) {
              return multiplier * -1
            } else if (_a > _b) {
              return multiplier * 1
            }
            return 0
          })
        } else { // custom
          // so we keep nulls at the end always when sorting
          const valDefault = multiplier > 0 ? "zzzzzzzzzz" : ""
          this.todos.sort(function(a, b) {
            const _a = (a.customAttributes[sortField] || valDefault)
            const _b = (b.customAttributes[sortField] || valDefault)
            if (_a < _b) {
              return multiplier * -1
            } else if (_a > _b) {
              return multiplier * 1
            }
            return 0
          })
        }

        this.sortMultiplier[sortField] = multiplier * -1

        return null
      },

      updateTodo: function(name) {
        return function() {
          console.log(name)
        }
      },

      toggleTodo: function(todo) {
        todo.selected = !todo.selected
      },

      stopPropagation: function(e) {
        e.stopPropagation()
      },

      editSeletedTodos: function() {
        $(".modal").addClass("is-active")
        this.hideDropdown()
      },

      closeModal: function() {
        $(".modal").removeClass("is-active")
      },

      deleteSeletedTodos: function() {
        if (confirm("Are you sure you want to delete these todo's?")) {
          $.ajax({
            url: "/todos/delete",
            type: "POST",
            dataType: "json",
            contentType: 'application/json',
            data: JSON.stringify({
              ids: this.todos.filter(t => t.selected).map(t => t.id)
            }),
            success: function(data){
              this.todos = this.todos.filter(function(t) {
                return !t.selected
              }.bind(this))
              this.hideDropdown()
            }.bind(this)
          })
        } else {
          console.log("no")
        }
      },

      selectAll: function() {
        this.todos.map(function(t) {
          t.selected = true
        })
        this.hideDropdown()
      },

      deselectAll: function() {
        this.todos.map(function(t) {
          t.selected = false
        })
        this.hideDropdown()
      },

      toggleMenuBurger: function(ev) {
        $(".navbar-burger").toggleClass("is-active")
        $(".navbar-menu").toggleClass("is-active")
      },

      hideDropdown: function(ev) {
        $(".navbar-menu").removeClass("is-active")
        $(".navbar-burger").removeClass("is-active")
      },

      getLicense: function() {
        const self = this
        self.loadingLicense = true
        $.ajax({
          url: "/license",
          type: "POST",
          dataType: "json",
          contentType: 'application/json',
          success: function(data){
            self.license = data.toodlesTier.tag
            self.loadingLicense = false
          },
          error: function(err){
            console.error(err)
          }
        })
      },

      submitTodoEdits: function(){
        this.keyValParseError = false
        this.nothingFilledError = false

        if (!(this.addKeyVals || this.setAssignee || this.setPriority || this.addTags)) {
          this.nothingFilledError = true
          return
        }
        const keyVals =
              this.addKeyVals.trim() === "" ?
              [] :
              this.addKeyVals.split(",").map(p => p.split("=").map(t => t.trim()))
        const keyValError = keyVals.some(p => p && p.length !== 2)
        if (keyValError) {
          this.addKeyValParseError = true
          return
        }

        $.ajax({
          url: "/todos/edit",
          type: "POST",
          dataType: "json",
          contentType: 'application/json',
          data: JSON.stringify({
            editIds: this.todos.filter(t => t.selected).map(t => t.id),
            setAssignee: this.setAssignee,
            addTags: this.addTags.split(",").map(s => s.trim()).filter(s => !!s)
              .map(tag => {
                return tag[0] === "#" ?
                  tag :
                  "#" + tag

              }),
            setPriority: parseInt(this.setPriority),
            addKeyVals: keyVals,
          }),
          success: function(data){
            this.todos.filter(function(t) {
              return t.selected
            }.bind(this))
              .map(function(t) {
                if (this.setAssignee) {
                  t.assignee = this.setAssignee
                }
                if (this.addTags) {
                  t.tags = t.tags.concat(this.addTags)
                }
                if (this.setPriority) {
                  t.priority = parseInt(this.setPriority)
                }

                if (this.addKeyVals) {
                  for (var i = 0; i < keyVals.length; i++) {
                    t.customAttributes[keyVals[i][0]] = keyVals[i][1]
                  }
                }
             }.bind(this))
            this.closeModal()
            this.todos.map(t => {
              t.selected = false
            })
          }.bind(this),
          error: console.log
        })
      }
    }
  })
})
