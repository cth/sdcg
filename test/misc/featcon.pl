resolve_conditioning_params([],[],[]).
resolve_conditioning_params([+|CMRest],[Param|ParamRest],[Param|CondParamRest]) :-
	resolve_conditioning_params(CMRest,ParamRest,CondParamRest).
resolve_conditioning_params([-|CMRest],[_|ParamRest],CondParamRest) :-
	resolve_conditioning_params(CMRest,ParamRest,CondParamRest).