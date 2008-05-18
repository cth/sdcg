expand_asserted_set(Name, NewValue) :-
	% The set clause might not have been asserted yet
	catch(Name(Values), _, true),
	
	(ground(Values) -> % if it has:
		retract(Name(Values)),
		%union([NewValue],Value,NewValues),
		NewClause =.. [ Name, [ NewValues ] ]
		; % otherwise
		NewClause =.. [ Name, [NewValue]]
	),
	assert(NewClause).
