let render=fun state body->if condition then(emit_line state;render_expr state body)else(emit_space state;render_expr state body;emit_space state)
