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
        todoSelected: {}
      }
    },
    created: function() {
      this.refresh()()
    },
    methods: {
      refresh: function(recompute) {
        return function() {
          this.loading = true
          $.ajax("http://localhost:9001/todos?recompute=" + !!recompute, {
            type: "GET",
            dataType: "json",
            success: function(data) {
              console.log("here", data)
              this.todos = data.todos.map(t => {
                return {
                  id: t.id,
                  assignee: t.assignee,
                  body: t.body.join("\n"),
                  lineNumber: t.lineNumber,
                  sourceFile: t.sourceFile,
                  priority: t.priority,
                  tags: t.tags,
                  customAttributes: t.customAttributes.reduce((acc, curr) => {
                    console.log(acc, curr)
                    acc[curr[0]] = curr[1]
                    console.log(acc, curr)
                    return acc
                  }, {})
                }
              })
              console.log("todos", this.todos)
              this.loading = false
              if (!recompute) {
                this.sortTodos('priority')
              }

              this.customAttributeKeys = Array.from(new Set([].concat.apply([], this.todos.map(t => {
                return Object.keys(t.customAttributes)
              }))))

              this.customAttributeKeys.sort()
            }.bind(this),
            error: function() {
              this.loading = false
              alert("Error! D: Check your connection to the server")
            }.bind(this)
          })
        }.bind(this)
      },

      // TODO(avi|p=2|key=a val) make sorts persist refreshes
      sortTodos: function(sortField) {
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
      deleteSeletedTodos: function() {
        if (confirm("Are you sure you want to delete these todo's?")) {
          $.ajax({
            url: "/todos/delete",
            type: "POST",
            dataType: "json",
            contentType: 'application/json',
            data: JSON.stringify({
              ids: Object.entries(this.todoSelected).filter(e => e[1]).map(e => parseInt(e[0]))
            }),
            success: function(data){
              this.todos = this.todos.filter(function(t) {
                return !this.todoSelected[t.id]
              }.bind(this))
              this.todoSelected = {}
            }.bind(this)
          })
        } else {
          console.log("no")
        }
      }
    }
  })
})

