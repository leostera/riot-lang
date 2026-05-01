module Std_order = Order

open Kernel

module type S = sig
  type key
  type +'value t

  val empty: 'value t

  val is_empty: 'value t -> bool

  val is_singleton: 'value t -> bool

  val singleton: key:key -> value:'value -> 'value t

  val insert: 'value t -> key:key -> value:'value -> 'value t

  val insert_to_list: 'value list t -> key:key -> value:'value -> 'value list t

  val update: 'value t -> key:key -> fn:('value option -> 'value option) -> 'value t

  val remove: 'value t -> key:key -> 'value t

  val merge:
    left:'left t ->
    right:'right t ->
    fn:(key:key -> left:'left option -> right:'right option -> 'merged option) ->
    'merged t

  val union:
    left:'value t ->
    right:'value t ->
    fn:(key:key -> left:'value -> right:'value -> 'value option) ->
    'value t

  val length: 'value t -> int

  val to_list: 'value t -> (key * 'value) list

  val from_list: (key * 'value) list -> 'value t

  val minimum: 'value t -> (key * 'value) option

  val minimum_unchecked: 'value t -> key * 'value

  val maximum: 'value t -> (key * 'value) option

  val maximum_unchecked: 'value t -> key * 'value

  val choose: 'value t -> (key * 'value) option

  val choose_unchecked: 'value t -> key * 'value

  val get: 'value t -> key:key -> 'value option

  val get_unchecked: 'value t -> key:key -> 'value

  val get_first: 'value t -> fn:(key -> bool) -> (key * 'value) option

  val get_first_unchecked: 'value t -> fn:(key -> bool) -> key * 'value

  val get_last: 'value t -> fn:(key -> bool) -> (key * 'value) option

  val get_last_unchecked: 'value t -> fn:(key -> bool) -> key * 'value

  val has_key: 'value t -> key:key -> bool

  val for_each: 'value t -> fn:(key -> 'value -> unit) -> unit

  val fold_left: 'value t -> init:'acc -> fn:('acc -> key -> 'value -> 'acc) -> 'acc

  val map: 'value t -> fn:('value -> 'mapped) -> 'mapped t

  val map_with_key: 'value t -> fn:(key -> 'value -> 'mapped) -> 'mapped t

  val filter: 'value t -> fn:(key -> 'value -> bool) -> 'value t

  val filter_map: 'value t -> fn:(key -> 'value -> 'mapped option) -> 'mapped t

  val partition: 'value t -> fn:(key -> 'value -> bool) -> 'value t * 'value t

  val split: 'value t -> key:key -> 'value t * 'value option * 'value t

  val equal: left:'value t -> right:'value t -> fn:('value -> 'value -> bool) -> bool

  val compare:
    left:'value t ->
    right:'value t ->
    fn:('value -> 'value -> Kernel.Order.t) ->
    Kernel.Order.t

  val all: 'value t -> fn:(key -> 'value -> bool) -> bool

  val any: 'value t -> fn:(key -> 'value -> bool) -> bool
end

module Make (Order: Std_order.Ordered) = struct
  type key = Order.t

  type +'value t =
    | Empty
    | Node of {
        left: 'value t;
        key: key;
        value: 'value;
        right: 'value t;
        height: int;
      }

  let panic = fun message -> Kernel.SystemError.panic ("Map: " ^ message)

  let height = fun __tmp1 ->
    match __tmp1 with
    | Empty -> 0
    | Node { height; _ } -> height

  let make_node = fun left ~key ~value right ->
    let left_height = height left in
    let right_height = height right in
    Node {
      left;
      key;
      value;
      right;
      height =
        if left_height >= right_height then
          left_height + 1
        else
          right_height + 1;
    }

  let singleton = fun ~key ~value ->
    Node {
      left = Empty;
      key;
      value;
      right = Empty;
      height = 1;
    }

  let balance = fun left ~key ~value right ->
    let left_height = height left in
    let right_height = height right in
    if left_height > right_height + 2 then
      match left with
      | Empty -> panic "balance expected a left branch"
      | Node {
          left = left_left;
          key = left_key;
          value = left_value;
          right = left_right;
          _;
        } ->
          if height left_left >= height left_right then
            make_node
              left_left
              ~key:left_key
              ~value:left_value
              (make_node left_right ~key ~value right)
          else
            match left_right with
            | Empty -> panic "balance expected a left-right branch"
            | Node {
                left = left_right_left;
                key = pivot_key;
                value = pivot_value;
                right = left_right_right;
                _;
              } ->
                make_node
                  (make_node left_left ~key:left_key ~value:left_value left_right_left)
                  ~key:pivot_key
                  ~value:pivot_value
                  (make_node left_right_right ~key ~value right)
    else if right_height > left_height + 2 then
      match right with
      | Empty -> panic "balance expected a right branch"
      | Node {
          left = right_left;
          key = right_key;
          value = right_value;
          right = right_right;
          _;
        } ->
          if height right_right >= height right_left then
            make_node
              (make_node left ~key ~value right_left)
              ~key:right_key
              ~value:right_value
              right_right
          else
            match right_left with
            | Empty -> panic "balance expected a right-left branch"
            | Node {
                left = right_left_left;
                key = pivot_key;
                value = pivot_value;
                right = right_left_right;
                _;
              } ->
                make_node
                  (make_node left ~key ~value right_left_left)
                  ~key:pivot_key
                  ~value:pivot_value
                  (make_node right_left_right ~key:right_key ~value:right_value right_right)
    else
      make_node left ~key ~value right

  let empty = Empty

  let is_empty = fun __tmp1 ->
    match __tmp1 with
    | Empty -> true
    | Node _ -> false

  let is_singleton = fun __tmp1 ->
    match __tmp1 with
    | Node { left = Empty; right = Empty; _ } -> true
    | Empty
    | Node _ -> false

  let rec insert = fun map ~key ~value ->
    match map with
    | Empty -> singleton ~key ~value
    | Node ({
      left;
      key = current_key;
      value = current_value;
      right;
      height;
    } as node) ->
        (
            match Order.compare key current_key with
            | Kernel.Order.EQ ->
                if Ptr.equal current_value value then
                  map
                else
                  Node { node with key; value }
            | Kernel.Order.LT ->
                let next_left = insert left ~key ~value in
                if Ptr.equal next_left left then
                  map
                else
                  balance next_left ~key:current_key ~value:current_value right
            | Kernel.Order.GT ->
                let next_right = insert right ~key ~value in
                if Ptr.equal next_right right then
                  map
                else
                  balance left ~key:current_key ~value:current_value next_right
          )

  and update = fun map ~key ~fn ->
    match map with
    | Empty -> (
        match fn None with
        | None -> Empty
        | Some value -> singleton ~key ~value
      )
    | Node ({
      left;
      key = current_key;
      value = current_value;
      right;
      height;
    } as node) ->
        (
            match Order.compare key current_key with
            | Kernel.Order.EQ -> (
                match fn (Some current_value) with
                | None -> merge_branches left right
                | Some value ->
                    if Ptr.equal current_value value then
                      map
                    else
                      Node { node with key; value }
              )
            | Kernel.Order.LT ->
                let next_left = update left ~key ~fn in
                if Ptr.equal next_left left then
                  map
                else
                  balance next_left ~key:current_key ~value:current_value right
            | Kernel.Order.GT ->
                let next_right = update right ~key ~fn in
                if Ptr.equal next_right right then
                  map
                else
                  balance left ~key:current_key ~value:current_value next_right
          )

  and minimum_unchecked = fun __tmp1 ->
    match __tmp1 with
    | Empty -> panic "minimum_unchecked called on an empty map"
    | Node { left = Empty; key; value; _ } -> (key, value)
    | Node { left; _ } -> minimum_unchecked left

  and maximum_unchecked = fun __tmp1 ->
    match __tmp1 with
    | Empty -> panic "maximum_unchecked called on an empty map"
    | Node { right = Empty; key; value; _ } -> (key, value)
    | Node { right; _ } -> maximum_unchecked right

  and remove_minimum = fun __tmp1 ->
    match __tmp1 with
    | Empty -> panic "remove_minimum called on an empty map"
    | Node { left = Empty; right; _ } -> right
    | Node {
        left;
        key;
        value;
        right;
        _;
      } ->
        balance (remove_minimum left) ~key ~value right

  and join = fun left ~key ~value right ->
    match (left, right) with
    | (Empty, _) -> add_minimum ~key ~value right
    | (_, Empty) -> add_maximum left ~key ~value
    | (
        Node {
          left = left_left;
          key = left_key;
          value = left_value;
          right = left_right;
          height = left_height;
        },
        Node {
          left = right_left;
          key = right_key;
          value = right_value;
          right = right_right;
          height = right_height;
        }
      ) ->
        if left_height > right_height + 2 then
          balance
            left_left
            ~key:left_key
            ~value:left_value
            (join left_right ~key ~value right)
        else if right_height > left_height + 2 then
          balance
            (join left ~key ~value right_left)
            ~key:right_key
            ~value:right_value
            right_right
        else
          make_node left ~key ~value right

  and add_minimum = fun ~key ~value map ->
    match map with
    | Empty -> singleton ~key ~value
    | Node {
        left;
        key = current_key;
        value = current_value;
        right;
        _;
      } ->
        balance
          (add_minimum ~key ~value left)
          ~key:current_key
          ~value:current_value
          right

  and add_maximum = fun map ~key ~value ->
    match map with
    | Empty -> singleton ~key ~value
    | Node {
        left;
        key = current_key;
        value = current_value;
        right;
        _;
      } ->
        balance
          left
          ~key:current_key
          ~value:current_value
          (add_maximum right ~key ~value)

  and concat = fun left right ->
    match (left, right) with
    | (Empty, map)
    | (map, Empty) -> map
    | _ ->
        let (key, value) = minimum_unchecked right in
        join left ~key ~value (remove_minimum right)

  and merge_branches = fun left right ->
    match (left, right) with
    | (Empty, map)
    | (map, Empty) -> map
    | _ ->
        let (key, value) = minimum_unchecked right in
        balance left ~key ~value (remove_minimum right)

  let insert_to_list = fun map ~key ~value ->
    let prepend = fun __tmp1 ->
      match __tmp1 with
      | None -> Some [ value ]
      | Some values -> Some (value :: values)
    in
    update map ~key ~fn:prepend

  let minimum = fun __tmp1 ->
    match __tmp1 with
    | Empty -> None
    | map -> Some (minimum_unchecked map)

  let maximum = fun __tmp1 ->
    match __tmp1 with
    | Empty -> None
    | map -> Some (maximum_unchecked map)

  let choose = minimum

  let choose_unchecked = minimum_unchecked

  let rec remove = fun map ~key ->
    match map with
    | Empty -> Empty
    | Node {
        left;
        key = current_key;
        value;
        right;
        _;
      } ->
        (
            match Order.compare key current_key with
            | Kernel.Order.EQ -> merge_branches left right
            | Kernel.Order.LT ->
                let next_left = remove left ~key in
                if Ptr.equal next_left left then
                  map
                else
                  balance next_left ~key:current_key ~value right
            | Kernel.Order.GT ->
                let next_right = remove right ~key in
                if Ptr.equal next_right right then
                  map
                else
                  balance left ~key:current_key ~value next_right
          )

  let rec split = fun map ~key ->
    match map with
    | Empty -> (Empty, None, Empty)
    | Node {
        left;
        key = current_key;
        value;
        right;
        _;
      } ->
        (
            match Order.compare key current_key with
            | Kernel.Order.EQ -> (left, Some value, right)
            | Kernel.Order.LT ->
                let (left_left, found, left_right) = split left ~key in
                (left_left, found, join left_right ~key:current_key ~value right)
            | Kernel.Order.GT ->
                let (right_left, found, right_right) = split right ~key in
                (join left ~key:current_key ~value right_left, found, right_right)
          )

  let concat_or_join = fun left ~key ~value right ->
    match value with
    | Some value -> join left ~key ~value right
    | None -> concat left right

  let rec merge = fun ~left ~right ~fn ->
    match (left, right) with
    | (Empty, Empty) -> Empty
    | (Node {
         left = left_left;
         key;
         value;
         right = left_right;
         height = left_height;
       }, _) when left_height >= height right ->
        let (right_left, right_value, right_right) = split right ~key in
        concat_or_join
          (merge ~left:left_left ~right:right_left ~fn)
          ~key
          ~value:(fn ~key ~left:(Some value) ~right:right_value)
          (merge ~left:left_right ~right:right_right ~fn)
    | (_, Node {
            left = right_left;
            key;
            value;
            right = right_right;
            _;
          }) ->
        let (left_left, left_value, left_right) = split left ~key in
        concat_or_join
          (merge ~left:left_left ~right:right_left ~fn)
          ~key
          ~value:(fn ~key ~left:left_value ~right:(Some value))
          (merge ~left:left_right ~right:right_right ~fn)
    | _ -> panic "merge reached an impossible state"

  let rec union = fun ~left ~right ~fn ->
    match (left, right) with
    | (Empty, map)
    | (map, Empty) -> map
    | (
        Node {
          left = left_left;
          key = left_key;
          value = left_value;
          right = left_right;
          height = left_height;
        },
        Node {
          left = right_left;
          key = right_key;
          value = right_value;
          right = right_right;
          height = right_height;
        }
      ) ->
        if left_height >= right_height then
          let (split_left, split_value, split_right) = split right ~key:left_key in
          let merged_left = union ~left:left_left ~right:split_left ~fn in
          let merged_right = union ~left:left_right ~right:split_right ~fn in
          match split_value with
          | None -> join merged_left ~key:left_key ~value:left_value merged_right
          | Some right_value ->
              concat_or_join
                merged_left
                ~key:left_key
                ~value:(fn ~key:left_key ~left:left_value ~right:right_value)
                merged_right
        else
          let (split_left, split_value, split_right) = split left ~key:right_key in
          let merged_left = union ~left:split_left ~right:right_left ~fn in
          let merged_right = union ~left:split_right ~right:right_right ~fn in
          match split_value with
          | None -> join merged_left ~key:right_key ~value:right_value merged_right
          | Some left_value ->
              concat_or_join
                merged_left
                ~key:right_key
                ~value:(fn ~key:right_key ~left:left_value ~right:right_value)
                merged_right

  let rec get = fun map ~key ->
    match map with
    | Empty -> None
    | Node {
        left;
        key = current_key;
        value;
        right;
        _;
      } ->
        (
            match Order.compare key current_key with
            | Kernel.Order.EQ -> Some value
            | Kernel.Order.LT -> get left ~key
            | Kernel.Order.GT -> get right ~key
          )

  let get_unchecked = fun map ~key ->
    match get map ~key with
    | Some value -> value
    | None -> panic "get_unchecked could not find the requested key"

  let rec get_first = fun map ~fn ->
    match map with
    | Empty -> None
    | Node {
        left;
        key;
        value;
        right;
        _;
      } ->
        if fn key then
          match get_first left ~fn with
          | Some _ as result -> result
          | None -> Some (key, value)
        else
          get_first right ~fn

  let get_first_unchecked = fun map ~fn ->
    match get_first map ~fn with
    | Some binding -> binding
    | None -> panic "get_first_unchecked could not find a matching key"

  let rec get_last = fun map ~fn ->
    match map with
    | Empty -> None
    | Node {
        left;
        key;
        value;
        right;
        _;
      } ->
        if fn key then
          match get_last right ~fn with
          | Some _ as result -> result
          | None -> Some (key, value)
        else
          get_last left ~fn

  let get_last_unchecked = fun map ~fn ->
    match get_last map ~fn with
    | Some binding -> binding
    | None -> panic "get_last_unchecked could not find a matching key"

  let has_key = fun map ~key ->
    match get map ~key with
    | Some _ -> true
    | None -> false

  let rec for_each = fun map ~fn ->
    match map with
    | Empty -> ()
    | Node {
        left;
        key;
        value;
        right;
        _;
      } ->
        for_each left ~fn;
        fn key value;
        for_each right ~fn

  let rec fold_left = fun map ~init ~fn ->
    match map with
    | Empty -> init
    | Node {
        left;
        key;
        value;
        right;
        _;
      } ->
        let acc = fold_left left ~init ~fn in
        let acc = fn acc key value in
        fold_left right ~init:acc ~fn

  let rec map = fun values ~fn ->
    match values with
    | Empty -> Empty
    | Node {
        left;
        key;
        value;
        right;
        height;
      } ->
        Node {
          left = map left ~fn;
          key;
          value = fn value;
          right = map right ~fn;
          height;
        }

  let rec map_with_key = fun values ~fn ->
    match values with
    | Empty -> Empty
    | Node {
        left;
        key;
        value;
        right;
        height;
      } ->
        Node {
          left = map_with_key left ~fn;
          key;
          value = fn key value;
          right = map_with_key right ~fn;
          height;
        }

  let rec filter = fun values ~fn ->
    match values with
    | Empty -> Empty
    | Node {
        left;
        key;
        value;
        right;
        _;
      } ->
        let next_left = filter left ~fn in
        let keep = fn key value in
        let next_right = filter right ~fn in
        if keep then
          join next_left ~key ~value next_right
        else
          concat next_left next_right

  let rec filter_map = fun values ~fn ->
    match values with
    | Empty -> Empty
    | Node {
        left;
        key;
        value;
        right;
        _;
      } ->
        let next_left = filter_map left ~fn in
        let mapped = fn key value in
        let next_right = filter_map right ~fn in
        match mapped with
        | Some value -> join next_left ~key ~value next_right
        | None -> concat next_left next_right

  let rec partition = fun values ~fn ->
    match values with
    | Empty -> (Empty, Empty)
    | Node {
        left;
        key;
        value;
        right;
        _;
      } ->
        let (left_true, left_false) = partition left ~fn in
        let keep = fn key value in
        let (right_true, right_false) = partition right ~fn in
        if keep then
          (join left_true ~key ~value right_true, concat left_false right_false)
        else
          (concat left_true right_true, join left_false ~key ~value right_false)

  type 'value enumeration =
    | End
    | More of key * 'value * 'value t * 'value enumeration

  let rec push_left = fun map enumeration ->
    match map with
    | Empty -> enumeration
    | Node {
        left;
        key;
        value;
        right;
        _;
      } ->
        push_left left (More (key, value, right, enumeration))

  let compare = fun ~left ~right ~fn ->
    let rec loop left right =
      match (left, right) with
      | (End, End) -> Kernel.Order.EQ
      | (End, _) -> Kernel.Order.LT
      | (_, End) -> Kernel.Order.GT
      | (
          More (left_key, left_value, left_right, left_rest),
          More (right_key, right_value, right_right, right_rest)
        ) -> (
          match Order.compare left_key right_key with
          | Kernel.Order.EQ -> (
              match fn left_value right_value with
              | Kernel.Order.EQ ->
                  loop (push_left left_right left_rest) (push_left right_right right_rest)
              | Kernel.Order.LT
              | Kernel.Order.GT as order -> order
            )
          | Kernel.Order.LT
          | Kernel.Order.GT as order -> order
        )
    in
    loop (push_left left End) (push_left right End)

  let equal = fun ~left ~right ~fn ->
    let rec loop left right =
      match (left, right) with
      | (End, End) -> true
      | (End, _)
      | (_, End) -> false
      | (
          More (left_key, left_value, left_right, left_rest),
          More (right_key, right_value, right_right, right_rest)
        ) -> (
          match Order.compare left_key right_key with
          | Kernel.Order.EQ ->
              fn left_value right_value
              && loop (push_left left_right left_rest) (push_left right_right right_rest)
          | Kernel.Order.LT
          | Kernel.Order.GT -> false
        )
    in
    loop (push_left left End) (push_left right End)

  let length =
    let rec loop = fun __tmp1 ->
      match __tmp1 with
      | Empty -> 0
      | Node { left; right; _ } -> loop left + 1 + loop right
    in
    loop

  let to_list =
    let rec loop acc = fun __tmp1 ->
      match __tmp1 with
      | Empty -> acc
      | Node {
          left;
          key;
          value;
          right;
          _;
        } ->
          loop ((key, value) :: loop acc right) left
    in
    fun map -> loop [] map

  let from_list = fun entries ->
    List.fold_left
      entries
      ~acc:empty
      ~fn:(fun map (key, value) ->
        insert map ~key ~value)

  let all = fun map ~fn -> fold_left map ~init:true ~fn:(fun acc key value -> acc && fn key value)

  let any = fun map ~fn -> fold_left map ~init:false ~fn:(fun acc key value -> acc || fn key value)
end
