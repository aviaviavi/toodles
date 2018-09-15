// TODO(avi) - add a refresh button
// TODO(avi) - add function to remove entries
// TODO(avi) - sorting functionality
$(document).ready(function() {
  new Vue({
    el: '#top-div',
    data: () => {
      return {
        todos: [],
        todoSearch: "",
        loading: true
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
            }.bind(this),
            error: function() {
              this.loading = false
              alert("Error! D: Check your connection to the server")
            }
          })
        }.bind(this)
      }
    }
  })
})

