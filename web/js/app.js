$(document).ready(function() {
  new Vue({
    el: '#container',
    data: () => {
      return {
        todos: []
      }
    },
    created: function() {
      $.ajax("http://localhost:9001/todos", {
        type: "GET",
        dataType: "json",
        success: function(data) {
          console.log("here", data)
          this.todos = data.todos.map(t => {
            return {
              assignee: t.assignee,
              body: t.body.join("\n")
            }
          })
          console.log("todos", this.todos)
        }.bind(this)
      })
    }
  })
})

