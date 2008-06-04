% Tense:
% Number: sing/plur
% past, present, future
% Person: first, second, third
% Valency: intransitive, transitive, ditransitive

% I may omit this;
% Continuity:
% 		Donna _appear_ confused.	: non-contionous.
%		*Donna _eats_ confused.	:

% English only shows distinctive agreement in the third person singular, present tense form of verbs 
% (which is marked by adding "-s"); the rest of the persons are not distinguished in the verb.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Common verbs, present tenses
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% These verbs can be used with either singular

present_tense_verb_base(investigate).
present_tense_verb_base(find).
present_tense_verb_base(act).
present_tense_verb_base(follow).
present_tense_verb_base(inure).
present_tense_verb_base(achieve).
present_tense_verb_base(reduce).
present_tense_verb_base(take).
present_tense_verb_base(remedy).
present_tense_verb_base(set).
present_tense_verb_base(distribute).
present_tense_verb_base(realize).
present_tense_verb_base(disable).
present_tense_verb_base(feel).
present_tense_verb_base(receive).
present_tense_verb_base(continue).
present_tense_verb_base(place).
present_tense_verb_base(protect).
present_tense_verb_base(eliminate).
present_tense_verb_base(elaborate).
present_tense_verb_base(work).
present_tense_verb_base(permit).
present_tense_verb_base(run).
present_tense_verb_base(enter).
present_tense_verb_base(force).

%verb(@number(Number),present,@not_person(third),@valency_not(ditransitive,Valency),investigate) ==> [investigate].
%verb(@number(Number),present,@not_person(third),@valency_all(Valency),find) ==> [find].
%verb(@number(Number),present,@not_person(third),@valency_not(ditransitive,Valency),act) ==> [find].
%verb(@number,present,@not_person(third),transitive,investigate) ==> [investigate].

verb(@number(N),present,@exclude(person,second,P),@present_tense_verb_base(V)) ==> [V].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Common verbs, present tense 3rd person
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
present_tense_third_person_verb(derserve,deserves).
present_tense_third_person_verb(believe,believes).
present_tense_third_person_verb(receive,receives).
present_tense_third_person_verb(take,takes).
present_tense_third_person_verb(go,goes).
present_tense_third_person_verb(expire,expires).
present_tense_third_person_verb(say,says).
present_tense_third_person_verb(opposes).
present_tense_third_person_verb(starts).
present_tense_third_person_verb(permits).
present_tense_third_person_verb(expects).
present_tense_third_person_verb(thinks).
present_tense_third_person_verb(faces).
present_tense_third_person_verb(votes).
present_tense_third_person_verb(teaches).
present_tense_third_person_verb(holds).
present_tense_third_person_verb(calls).
present_tense_third_person_verb(fears).
present_tense_third_person_verb(spends).
present_tense_third_person_verb(collects).
present_tense_third_person_verb(backs).
present_tense_third_person_verb(eliminates).
present_tense_third_person_verb(sets).
present_tense_third_person_verb(flies).
present_tense_third_person_verb(gives).
present_tense_third_person_verb(seeks).
present_tense_third_person_verb(reads).

verb(sing,present,third,@present_tense_thirdverb(V)) ==> [V].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Some past tense verbs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
past_tense_verb(said).
past_tense_verb(produced).
past_tense_verb(took).
past_tense_verb(recommended).
past_tense_verb(commented).
past_tense_verb(urged).
past_tense_verb(found).
past_tense_verb(added).
past_tense_verb(praised).
past_tense_verb(charged).
past_tense_verb(listed).
past_tense_verb(became).
past_tense_verb(announced).
past_tense_verb(brought).
past_tense_verb(attended).
past_tense_verb(wanted).
past_tense_verb(voted).
past_tense_verb(defeated).
past_tense_verb(received).
past_tense_verb(got).
past_tense_verb(stood).
past_tense_verb(shot).
past_tense_verb(scheduled).
past_tense_verb(feared).
past_tense_verb(promised).

% A rule for representing past tense verbs
verb(@number(N),past,@person(P),@past_tense_verb(V)) ==> [V].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Past participle
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% VBN in Brown
past_participle_verb(conducted).
past_participle_verb(charged).
past_participle_verb(won).
past_participle_verb(received).
past_participle_verb(studied).
past_participle_verb(revised).
past_participle_verb(operated).
past_participle_verb(accepted).
past_participle_verb(combined).
past_participle_verb(experienced).
past_participle_verb(recommended).
past_participle_verb(effected).
past_participle_verb(granted).
past_participle_verb(seen).
past_participle_verb(protected).
past_participle_verb(adopted).
past_participle_verb(retarded).
past_participle_verb(notarized).
past_participle_verb(selected).
past_participle_verb(composed).
past_participle_verb(gotten).
past_participle_verb(printed).

% A rule for representing past tense verbs.
verb(@number(N),past-participle,@person(P),@past_participle_verb(V)) ==> [V].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Gerund verbs
% Gerund verbs have no additional features (other than begin gerund)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% VBG corresponds to gerunds in Brown

gerund_verb(being)
gerund_verb(preferring).
gerund_verb(arriving).
gerund_verb(requiring).
gerund_verb(leaving).
gerund_verb(modernizing).
gerund_verb(improving).
gerund_verb(purchasing).
gerund_verb(lacking).
gerund_verb(enabling).
gerund_verb(pricing).
gerund_verb(keeping).
gerund_verb(getting).
gerund_verb(picking).
gerund_verb(entering).
gerund_verb(voting).
gerund_verb(warning).
gerund_verb(making).
gerund_verb(strengthening).
gerund_verb(setting).
gerund_verb(neighboring).
gerund_verb(attending).
gerund_verb(participating).
gerund_verb(moving).

% A rule for gerund verbs
gerund_verb(@gerund_verb(V)) ==> [V].

% auxilary verbs
aux ==> [ do ].
aux ==> [ does ].
aux ==> [ can ].
aux ==> [ will ].

% From Brown. There is seems to be no tense distinction :-(
% e.g. "will" and "would" are both under the same tag (MD)
aux(should).
aux(may).
aux(might).
aux(will).
aux(would).
aux(must).
aux(can).
aux(could).
aux(shall).
aux(ought).