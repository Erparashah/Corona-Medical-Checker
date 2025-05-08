# Corona-Medical-Checker
% Expert System for Medical Diagnosis (Future Scope Enhanced)

:- style_check(-singleton).
:- dynamic has/2.  % has(Symptom, Severity)

% Entry point
start :-
    clear_facts,
    nl, write('--- Medical Diagnosis Expert System ---'), nl,
    ask_user_info,
    ask_symptoms,
    diagnose(Disease, Score),
    handle_diagnosis(Disease, Score),
    !.

% Clear previous user facts
clear_facts :- retractall(has(_, _)).

% Ask basic user info (for future risk-based diagnostics)
ask_user_info :-
    write('Please enter your age: '),
    read(_Age),
    nl.

% List of symptoms
symptom(fever).
symptom(cough).
symptom(fatigue).
symptom(loss_of_taste_smell).
symptom(shortness_of_breath).
symptom(headache).
symptom(sore_throat).
symptom(chest_pain).

% Ask for symptom and its severity
ask_symptoms :-
    forall(symptom(S),
        (
            format('Do you have ~w? (none/mild/moderate/severe): ', [S]),
            read(Severity),
            (Severity \= none -> assert(has(S, Severity)) ; true)
        )
    ).

% Weighting severity to a score
severity_score(mild, 1).
severity_score(moderate, 2).
severity_score(severe, 3).

% Count matching symptoms with weight for diagnosis
count_score([], 0).
count_score([Symptom|Rest], TotalScore) :-
    (has(Symptom, Level), severity_score(Level, S) -> true ; S = 0),
    count_score(Rest, RestScore),
    TotalScore is S + RestScore.

% Diagnosis rules with symptoms
disease(covid19, [fever, cough, fatigue, loss_of_taste_smell, shortness_of_breath]).
disease(flu, [fever, cough, fatigue, headache, loss_of_taste_smell]).
disease(common_cold, [cough, sore_throat, headache]).
disease(possible_chest_infection, [chest_pain, fever, cough]).
disease(healthy, []).

% Diagnose with the highest matching score
diagnose(BestDisease, BestScore) :-
    findall((D, S),
        (disease(D, Symptoms), count_score(Symptoms, S), S > 0),
        Scores),
    sort(2, @>=, Scores, Sorted),
    Sorted = [(BestDisease, BestScore)|_].

% Diagnosis output with score explanation
handle_diagnosis(healthy, 0) :-
    nl, write('Diagnosis: You appear to be healthy.'), nl,
    write('Recommendation: Stay hydrated and maintain hygiene.'), nl.

handle_diagnosis(Disease, Score) :-
    format('Diagnosis Suggestion: You may have ~w.', [Disease]), nl,
    format('Matching symptom score: ~w', [Score]), nl,
    write('Recommendation: Please consult a medical professional for accurate diagnosis.'), nl.
