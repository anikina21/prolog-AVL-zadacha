% Вузол AVL-дерева представлений у вигляді node(Ключ, ЛівеПіддерево, ПравеПіддерево, Висота)

% Порожнє дерево
empty(nil).

% Отримання висоти дерева
height(nil, 0).
height(node(_, L, R, H), H) :- height(L, HL), height(R, HR), H is max(HL, HR) + 1.

% Обертання ліворуч
rotate_left(node(K, L, node(RK, RL, RR, _), _), node(RK, node(K, L, RL, _), RR, H)) :-
    height(node(K, L, RL, _), HL),
    height(RR, HR),
    H is max(HL, HR) + 1.

% Обертання праворуч
rotate_right(node(K, node(LK, LL, LR, _), R, _), node(LK, LL, node(K, LR, R, _), H)) :-
    height(node(K, LR, R, _), HR),
    height(LL, HL),
    H is max(HL, HR) + 1.

% Балансування дерева
balance(node(K, L, R, _), BalancedTree) :-
    height(L, HL), height(R, HR),
    D is HL - HR,
    (D > 1 -> (L = node(_, LL, LR, _), height(LL, HLL), height(LR, HLR),
                (HLL >= HLR -> rotate_right(node(K, L, R, _), BalancedTree)
                ; rotate_left(L, NewL), rotate_right(node(K, NewL, R, _), BalancedTree)));
     D < -1 -> (R = node(_, RL, RR, _), height(RL, HRL), height(RR, HRR),
                (HRR >= HRL -> rotate_left(node(K, L, R, _), BalancedTree)
                ; rotate_right(R, NewR), rotate_left(node(K, L, NewR, _), BalancedTree)));
     BalancedTree = node(K, L, R, _)),
    height(BalancedTree, _).

% Вставка елемента в дерево
insert(nil, Key, node(Key, nil, nil, 1)).
insert(node(K, L, R, H), Key, BalancedTree) :-
    (Key < K -> insert(L, Key, NL), balance(node(K, NL, R, H), BalancedTree);
     Key > K -> insert(R, Key, NR), balance(node(K, L, NR, H), BalancedTree);
     BalancedTree = node(K, L, R, H)).

% Виведення дерева у вигляді структури
print_tree(Tree) :-
    write_tree(Tree, 0).

write_tree(nil, _).
write_tree(node(K, L, R, _), Depth) :-
    Depth1 is Depth + 4,
    write_tree(R, Depth1),
    tab(Depth), write(K), nl,
    write_tree(L, Depth1).

% Додавання списку елементів у дерево
insert_list(Tree, [], Tree).
insert_list(Tree, [H|T], ResultTree) :-
    insert(Tree, H, NewTree),
    insert_list(NewTree, T, ResultTree).

% Перетворення дерева в список (LNR обход)
tree_to_list(nil, []).
tree_to_list(node(K, L, R, _), List) :-
    tree_to_list(L, LL),
    tree_to_list(R, RL),
    append(LL, [K|RL], List).

% Видалення мінімального вузла з піддерева
delete_min(node(K, nil, R, _), K, R).
delete_min(node(K, L, R, _), Min, node(K, NL, R, _)) :-
    delete_min(L, Min, NL).

% Видалення елемента з дерева
delete(nil, _, nil).
delete(node(Key, L, R, _), Key, BalancedTree) :-
    (R = nil -> BalancedTree = L;
     L = nil -> BalancedTree = R;
     delete_min(R, Min, NewR), balance(node(Min, L, NewR, _), BalancedTree)).
delete(node(K, L, R, H), Key, BalancedTree) :-
    (Key < K -> delete(L, Key, NL), balance(node(K, NL, R, H), BalancedTree);
     Key > K -> delete(R, Key, NR), balance(node(K, L, NR, H), BalancedTree)).
