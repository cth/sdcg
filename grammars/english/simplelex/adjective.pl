%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Adjectives
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

adjective(expensive).
adjective(cheap).
adjective(cheapest).
adjective(nonstop).
adjective(first).
adjective(latest).
adjective(other).
adjective(direct).

%adj_str(A) :-q_stringify(adjective,A).

adjective(@enum(adjective,Stem)) ==> [Stem].