Representing erlang term in JSON.

```
<<"a">>			"a"
a			{"t":"atom", "v":"a"}
{1,2}			{"t":"tuple", "v":[1,2]}
#{a => 1}		{"t":"map",
			 "v": [{"t":"tuple",
				"v":[{"t": "atom", "v": "a"}, 1]}]}
[1,2,3]			[1,2,3]
```

