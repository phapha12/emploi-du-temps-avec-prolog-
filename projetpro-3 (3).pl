:- use_module(library(clpfd)).
:- use_module(library(lists)).

nb_cours(16).

nb_demiJours(4).

cours(1, 4, 4, '2h00', sc, 'Statistiques').
cours(2, 9, 4, '2h00', sc, 'XML').
cours(3, 2, 4, '2h00', si, 'PPC').
cours(4, 3, 4, '3h00', sc, 'Optim').
cours(5, 10, 4, '2h00', sc, 'Anglais').
cours(6, 7, 4, '2h15', sc, 'PPE').
cours(7, 8, 3, '2h15', sc, 'IOC').
cours(8, 4, 3, '2h00', sc, 'Statistiques').
cours(9, 2, 3, '2h00', si, 'Prolog').
cours(10, 5, 3, '2h00', sc, 'LLA').
cours(11, 2, 2, '2h00', sc, 'IHM').
cours(12, 6, 2, '2h00', sc, 'Analyse').
cours(13, 2, 2, '2h00', ts, 'ASD').
cours(14, 2, 1, '2h00', si, 'ProjetInfo').
cours(15, 1, 1, '2h00', sc, 'Algebre').
cours(16, 2, 1, '2h00', si, 'POO').

nb_profs(10).

prof(1, 'Rullier').
prof(2, 'Nguyen').
prof(3, 'Pinson').
prof(4, 'Marion').
prof(5, 'Peridy').
prof(6, 'Guergueb').
prof(7, 'Boivin').
prof(8, 'Rivreau').
prof(9, 'Rivault').
prof(10, 'Scotto').


nb_salles(5).

salle(1, 30, sc, 'B313').
salle(2, 50, sc, 'B317').
salle(3, 50, sc, 'C325').
salle(4, 50, si, 'C017').
salle(5, 50, si, 'C018').

nb_groupes(4).

groupe(1, 'L1', 50).
groupe(2, 'L2', 20).
groupe(3, 'L3', 25).
groupe(4, 'M1', 12).


make_time_limits([], []).
make_time_limits([N|Ns], [B,E|Ts]) :-
    (   N mod 2 =:= 1
    ->  B is 32+(N//2)*4*24,
        E is 50 + (N//2)*4*24
    ;   B is  54 + (N//2 -1)*4*24,
        E is 72 + (N//2 -1)*4*24
%**     Les calculs ne sont pas corrects !
    ),
    make_time_limits(Ns, Ts).
make_doms_and_breaks([B,E],B..E,[]).
make_doms_and_breaks([B1,E1,B2,E2|Ts],B1..E1 \/ Ds, [E1,B2|Bs]) :-
    make_doms_and_breaks([B2,E2|Ts],Ds,Bs).

atom_to_hour(A, H, M) :-
    atom_codes(A, L),
    codes_to_hour(L, H, M).


codes_to_hour(L, H, M) :-
    append(CH, [104|CM], L),
    number_codes(H, CH),
    number_codes(M, CM).

atom_to_quart(A, Q):-
atom_to_hour(A, H, M),
Q is (H*60+M)//15.

quart_to_hour(Q,H,M) :-
    A = Q mod 96,
    H is A//4,
    M is (A mod 4)*15.

courseLengths(Ds) :-
    findall(Q, (cours(_,_,_,D,_,_),atom_to_quart(D,Q)), N),
    Ds=N.

/*
 * break_start_length(Breaks, BreakStarts, BreakLengths)
 * qui à partir de Breaks = [50,54,72,128,...]
 * renvoie BreakStarts = [50,72,...] la liste des d�buts
 * et BreakLengths = [4,56,...] la liste des dur�es des pauses midi et
 nuit */

break_start_length([],[],[]).
break_start_length([B,E|Breaks], [B|Bstarts], [Bl|L]) :-
    Bl is E-B,
    break_start_length(Breaks,Bstarts,L).



/*
 * not_on_breaks(H, D, BreakStarts, BreakLengths)
 * qui pose la contrainte que le cours commençant à H et de durée D !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 * est disjoint des pauses définies par BreakStarts et BreakLengths
 */

not_on_breaks(H,D,BreakStarts,BreakLengths) :-
serialized([H|BreakStarts],[D|BreakLengths]).




teacherCourses(P, Nums) :-
    findall(N, cours(N,P,_,_,_,_), Nums).

groupcourses(Gr, Nm):-
    findall(N, cours(N,_,Gr,_,_,_), Nm).


/*
 * getByIndex([I1,I2,...,In], List, Xs) renvoie Xs = [X1,X2,...,Xn]
 * où X1 est le I1-ème élément de List, X2 est le I2-ème élément de List, ...
 */

getByIndex([],_,[]).
getByIndex([I1|In],List,[X1|Xs]):-
    nth1(I1,List,X1),
    getByIndex(In,List,Xs).


/*
 * make_breaks([H1,H2,...,Hn], [D1,D2,...,Dn], BreakStarts, BreakLengths)
 * renvoie BreakStarts = [S1,S2,...,Sn] tel que Si #= Hi + Di
 * et BreakLengths = [1,1,...,1].
 *
 * [H1,H2,...,Hn] et [D1,D2,...,Dn] sont les débuts et durées des cours
 * auxquels on ajoute les pauses.
 */
make_breaks([], [], [], []).
make_breaks([H|Hs], [D|Ds], [B|Bs], [L|Ls]):-
    B is H + D,
    L = 1,
    make_breaks(Hs, Ds, Bs , Ls).

add_break([],[]).
add_break([D1|Dn],[X|DBs]):-
    X is D1+1,
    add_break(Dn,DBs).

disjoint_courses_and_breaks(Nums, AllHs, AllDs):-
getByIndex(Nums,AllHs,Hs),
getByIndex(Nums,AllDs,Ds),
add_break(Ds,Dls),
serialized(Hs,Dls).

pcRooms(Nums) :-
    findall(N,salle(N,_,'si',_),Nums).



/*
 * boardRooms(Nums)
 * renvoie la liste Nums des num�ros des salles dont le type est sc
 */

boardRooms(Nums) :-
    findall(N,salle(N,_,'sc',_),Nums).

/*
 * roomCapacities(Caps)
 * renvoie la liste Caps des capacit�s de toutes les salles
 */


roomCapacities(Caps):-
     findall(C,salle(_,C,_,_),Caps).

listToRange([X],X).
listToRange([X|Xs],X\/Range) :-
    listToRange(Xs,Range).



fitNbStudents(I,S,Caps):-
    element(S,Caps,C),
    cours(I,_,G,_,_,_),
    groupe(G,_,Nb),
    C #>= Nb.





def_dom_salles([], [], _, _, _).
def_dom_salles([Num|NumsCours], [S|Ss], BoardDom, PCDom, NbSalles) :-
    cours(Num, _, _, _, Type, _),
    (   Type = 'sc', S in BoardDom
    ;   Type = 'si', S in PCDom
    ;   Type = 'ts', S in 1..NbSalles
    ),
    def_dom_salles(NumsCours, Ss, BoardDom, PCDom, NbSalles).


/*Pose les contraintes que tout les cours d'heure de d�but dans Hs et de duree dans Ds
 *soient disjoints des pauses definies par BreakStarts et BreakLengths*/

all_not_on_breaks([], [], _, _).
all_not_on_breaks([H|Hs],[D|Ds],BreakStarts,BreakLengths) :-
    not_on_breaks(H,D,BreakStarts,BreakLengths),
    all_not_on_breaks(Hs,Ds,BreakStarts,BreakLengths).


/*Pose la contrainte que chaque cours d'un prof soient disjoints d'au moins 15min
pour chaque prof*/

cours_prof_disjoints([],_,_).
cours_prof_disjoints([N|NumsProf],Hs,Ds) :-
    teacherCourses(N,Nums),
    disjoint_courses_and_breaks(Nums,Hs,Ds),
    cours_prof_disjoints(NumsProf,Hs,Ds).


/*Pose la contrainte que chaque cours d'un groupe soient disjoints d'au moins 15min
pour chaque groupe*/

cours_groupe_disjoints([],_,_).
cours_groupe_disjoints([N|NumsGroupes],Hs,Ds):-
    groupCourses(N,Nums),
    disjoint_courses_and_breaks(Nums,Hs,Ds),
    cours_groupe_disjoints(NumsGroupes,Hs,Ds).

/**/

capacite_salles([],[],_).
capacite_salles([N|NumsCours],[S|Ss],Caps):-
    fitNbStudents(N,S,Caps),
    capacite_salles(NumsCours,Ss,Caps).

/**/

make_rect([], [], [], [], _).
make_rect([X|Xs], [L|Ls], [Y|Ys], [H|Hs], [r(X,L,Y,H)|Rs]) :-
    make_rect(Xs, Ls, Ys, Hs, Rs).

/**/

disjoint(r(X1,L1,Y1,H1), r(X2,L2,Y2,H2)) :-
    X1 + L1 #=< X2 #\/ X2 + L2 #=< X1 #\/ Y1 + H1 #=< Y2 #\/ Y2 + H2 #=< Y1.

/**/

disjoint_de_tous(_,[]).
disjoint_de_tous(R,[P|Ps]):-
    disjoint(R,P),
    disjoint_de_tous(R,Ps).


/**/

rec_disjoints([]).
rec_disjoints([R|Rs]):-
    disjoint_de_tous(R,Rs),
    rec_disjoints(Rs).





solver(Sol):-
    %consult(Fichiers),

% Creer les variables Hs = [H1,H2,...], Ss = [S1,S2,...]
    nb_cours(NbCours),
    length(Hs, NbCours),
    length(Ss, NbCours),

    % Definir le domaine des Hs
    nb_demiJours(NbDmj),
    numlist(1, NbDmj, NumsDMJ),
    make_time_limits(NumsDMJ, Limits),
    make_doms_and_breaks(Limits, Doms, Breaks),       % Doms = 32..50 \/ 54..72 ...
    Hs ins Doms,

    % Definir le domaine des Ss
    % Trouver le numeros des salles de cours et creer l'union de ces numeros
    % Trouver le numeros des salles info et creer l'union de ces numeros
    % Trouver le nombre de salles
    % Parcourir tous les cours pour dinir


    boardRooms(BoardNums), listToRange(BoardNums, BoardDom),
    pcRooms(PCNums), listToRange(PCNums, PCDom),
    nb_salles(NbSalles),
    numlist(1, NbCours, NumsCours),                    % NumsCours = [1,2,...,NbCours]
    def_dom_salles(NumsCours, Ss, BoardDom, PCDom, NbSalles),


    %Ss ins 1..NbCours,


    % Le creneau de tout cours est disjoint des creneaux de pause
    % Creer la liste des debuts et la liste des durees des pauses
    % Recuper la durees des cours

    break_start_length(Breaks, BreakStarts, BreakLengths),
    courseLengths(Ds),
    all_not_on_breaks(Hs, Ds, BreakStarts, BreakLengths),

    % Les cours d'un prof avec 15' de pause sont disjoint
    % Pour chaque prof : Recuperer le numero de ses cours
    % Y apppliquer disjoint_courses_and_breaks


    nb_profs(NbProfs),
    numlist(1,NbProfs, NumsProf),
    cours_prof_disjoints(NumsProf, Hs, Ds),

    % Idem pour les groupes

    nb_groupes(NbGroupes),
    numlist(1,NbGroupes,NumsGroupes),
    cours_groupe_disjoints(NumsGroupes,Hs,Ds),

    % Capacite de la salle est superieure au nombre d'etudiants de chaque cours
    % Utiliser NumsCours, roomCapacities, fitNbStudents
    %Recuperer le cours pour connaitre le cours et le nb etudiant, et appliquer fitNbstudent

    roomCapacities(Caps),
    capacite_salles(NumsCours,Ss,Caps),



    % Les rectangles representant les cours sont disjoints
    % make_rect, disjoint2 ou rect_disjoints
    % cours=[c(NumCours, NumProf, NumGroupe, Duree, TypeSalle, NomCours)
    Dix1=[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
    make_rect(Hs,Ds,Ss,Dix1,Rectangle),
    rec_disjoints(Rectangle),


    %Labeling
    append(Hs,Ss,Sol),
    labeling([],Sol).

    % Affichage
    % Exemple H = 32 => Afficher 8h00 Lundi
    %         H = 128 => Afficher 8h00 Mardi
    % Nom de cours, salle de cours : S = 3 alors C325
    %open(File,write,Stream),
    %export(NumsCours,Hs,Ss,Stream),
    %close(Stream).


print([],[],[]).
print([N|NumsCours],[H|Hs],[S|Ss]) :-
    quart_to_hour(H,Hh,Hm),
    (
     (Jour = 'Lundi', H<96);
     (Jour = 'Mardi', H>=96, H<192);
     (Jour = 'Mercredi', H>=192, H<288);
     (Jour = 'Jeudi', H>=288, H<384);
     (Jour = 'Vendredi', H>=384)
    ),
    salle(S,_,_,Salle),
    atomics_to_string([Jour,' ',Hh,'h',Hm,' ',Salle],StrDate),
    write(StrDate), nl(),

    cours(N,NumP,NumG,_,_,NomC),
    prof(NumP,NomP),
    groupe(NumG,NomG,_),
    atomics_to_string([NomG,' ',NomC,' ',NomP],StrCours),
    write(StrCours), nl(), nl(),
    print(NumsCours,Hs,Ss).



/*Separe une liste [A1..An|B1..Bn] en 2 listes [A1..An] et [B1..Bn]*/

split(L, A, B):-
    append(A, B, L),
    same_length(A, B),
    !.

/*Affiche le planning dans la console*/

emploi_temps() :-
    nb_cours(NbCours),
    numlist(1, NbCours, NumsCours),
    solver(Sol),
    split(Sol,Hs,Ss),
    print(NumsCours,Hs,Ss).






