
// var classes = ["color","size","material","texture","age","quality","shape"]; // old classes
var classes = ["age","color","dimension","human","location","material","nationality","physical","shape","speed","temporal","value"];

// var adjectives = [ // old adjectives
// 		{"Predicate":"red", "Class":"color"},
// 		{"Predicate":"yellow", "Class":"color"},
// 		{"Predicate":"green", "Class":"color"},
// 		{"Predicate":"blue", "Class":"color"},
// 		{"Predicate":"purple", "Class":"color"},
// 		{"Predicate":"brown", "Class":"color"},											
// 		{"Predicate":"big", "Class":"size"},
// 		{"Predicate":"small", "Class":"size"},					
// 		{"Predicate":"huge", "Class":"size"},					
// 		{"Predicate":"tiny", "Class":"size"},					
// 		{"Predicate":"short", "Class":"size"},					
// 		{"Predicate":"long", "Class":"size"},							
// 		{"Predicate":"wooden", "Class":"material"},
// 		{"Predicate":"plastic", "Class":"material"},
// 		{"Predicate":"metal", "Class":"material"},
// 		{"Predicate":"smooth", "Class":"texture"},
// 		{"Predicate":"hard", "Class":"texture"},
// 		{"Predicate":"soft", "Class":"texture"},
// 		{"Predicate":"old", "Class":"age"},
// 		{"Predicate":"new", "Class":"age"},
// 		{"Predicate":"rotten", "Class":"age"},
// 		{"Predicate":"fresh", "Class":"age"},
// 		{"Predicate":"good", "Class":"quality"},
// 		{"Predicate":"bad", "Class":"quality"},
// 		{"Predicate":"round", "Class":"shape"},						
// 		{"Predicate":"square", "Class":"shape"}
// ];
var adjectives = [
		{	"Predicate":"old",	"Class":"age"	},
		{	"Predicate":"new",	"Class":"age"	},
		{	"Predicate":"junior",	"Class":"age"	},
		{	"Predicate":"young",	"Class":"age"	},
		{	"Predicate":"senior",	"Class":"age"	},
		{	"Predicate":"old-time",	"Class":"age"	},
		{	"Predicate":"blue",	"Class":"color"	},
		{	"Predicate":"white",	"Class":"color"	},
		{	"Predicate":"red",	"Class":"color"	},
		{	"Predicate":"blonde",	"Class":"color"	},
		{	"Predicate":"black",	"Class":"color"	},
		{	"Predicate":"purple",	"Class":"color"	},
		{	"Predicate":"green",	"Class":"color"	},
		{	"Predicate":"yellow",	"Class":"color"	},
		{	"Predicate":"full",	"Class":"dimension"	},
		{	"Predicate":"lengthy",	"Class":"dimension"	},
		{	"Predicate":"short",	"Class":"dimension"	},
		{	"Predicate":"little",	"Class":"dimension"	},
		{	"Predicate":"big",	"Class":"dimension"	},
		{	"Predicate":"long",	"Class":"dimension"	},
		{	"Predicate":"fat",	"Class":"dimension"	},
		{	"Predicate":"high",	"Class":"dimension"	},
		// {	"Predicate":"skinny",	"Class":"dimension"	},
		// {	"Predicate":"tall",	"Class":"dimension"	},
		// {	"Predicate":"small",	"Class":"dimension"	},
		// {	"Predicate":"large",	"Class":"dimension"	},
		// {	"Predicate":"tiny",	"Class":"dimension"	},
		// {	"Predicate":"wide",	"Class":"dimension"	},
		// {	"Predicate":"mini",	"Class":"dimension"	},
		// {	"Predicate":"flat",	"Class":"dimension"	},
		// {	"Predicate":"open",	"Class":"dimension"	},
		// {	"Predicate":"narrow",	"Class":"dimension"	},
		// {	"Predicate":"humongous",	"Class":"dimension"	},
		// {	"Predicate":"biggest",	"Class":"dimension"	},
		// {	"Predicate":"huge",	"Class":"dimension"	},
		// {	"Predicate":"thick",	"Class":"dimension"	},
		// {	"Predicate":"low",	"Class":"dimension"	},
		// {	"Predicate":"deep",	"Class":"dimension"	},
		// {	"Predicate":"tiniest",	"Class":"dimension"	},
		// {	"Predicate":"slight",	"Class":"dimension"	},
		// {	"Predicate":"thin",	"Class":"dimension"	},
		{	"Predicate":"creative",	"Class":"human"	},
		{	"Predicate":"elitist",	"Class":"human"	},
		{	"Predicate":"strict",	"Class":"human"	},
		{	"Predicate":"hopeless",	"Class":"human"	},
		{	"Predicate":"crazy",	"Class":"human"	},
		{	"Predicate":"social",	"Class":"human"	},
		{	"Predicate":"experienced",	"Class":"human"	},
		{	"Predicate":"selfish",	"Class":"human"	},
		// {	"Predicate":"happiest",	"Class":"human"	},
		// {	"Predicate":"professional",	"Class":"human"	},
		// {	"Predicate":"funny",	"Class":"human"	},
		// {	"Predicate":"intellectual",	"Class":"human"	},
		// {	"Predicate":"entrepreneurial",	"Class":"human"	},
		// {	"Predicate":"mad",	"Class":"human"	},
		// {	"Predicate":"civilized",	"Class":"human"	},
		// {	"Predicate":"sad",	"Class":"human"	},
		// {	"Predicate":"playful",	"Class":"human"	},
		// {	"Predicate":"cheerful",	"Class":"human"	},
		{	"Predicate":"overhead",	"Class":"location"	},
		{	"Predicate":"internal",	"Class":"location"	},
		{	"Predicate":"closest",	"Class":"location"	},
		{	"Predicate":"corduroy",	"Class":"material"	},
		{	"Predicate":"wooden",	"Class":"material"	},
		{	"Predicate":"gold",	"Class":"material"	},
		{	"Predicate":"crocheted",	"Class":"material"	},
		{	"Predicate":"brazilian",	"Class":"nationality"	},
		{	"Predicate":"foreign",	"Class":"nationality"	},
		{	"Predicate":"texan",	"Class":"nationality"	},
		{	"Predicate":"american",	"Class":"nationality"	},
		{	"Predicate":"hispanic",	"Class":"nationality"	},
		{	"Predicate":"argentine",	"Class":"nationality"	},
		{	"Predicate":"chinese",	"Class":"nationality"	},
		{	"Predicate":"victorian",	"Class":"nationality"	},
		// {	"Predicate":"english",	"Class":"nationality"	},
		// {	"Predicate":"french",	"Class":"nationality"	},
		// {	"Predicate":"mexican",	"Class":"nationality"	},
		// {	"Predicate":"italian",	"Class":"nationality"	},
		// {	"Predicate":"vietnamese",	"Class":"nationality"	},
		// {	"Predicate":"international",	"Class":"nationality"	},
		// {	"Predicate":"national",	"Class":"nationality"	},
		// {	"Predicate":"japanese",	"Class":"nationality"	},
		// {	"Predicate":"european",	"Class":"nationality"	},
		// {	"Predicate":"native",	"Class":"nationality"	},
		// {	"Predicate":"latin",	"Class":"nationality"	},
		{	"Predicate":"hard",	"Class":"physical"	},
		{	"Predicate":"clean",	"Class":"physical"	},
		{	"Predicate":"sweet",	"Class":"physical"	},
		{	"Predicate":"creamy",	"Class":"physical"	},
		{	"Predicate":"knobby",	"Class":"physical"	},
		{	"Predicate":"salty",	"Class":"physical"	},
		{	"Predicate":"wet",	"Class":"physical"	},
		{	"Predicate":"spiky",	"Class":"physical"	},
		// {	"Predicate":"shiny",	"Class":"physical"	},
		// {	"Predicate":"raspy",	"Class":"physical"	},
		// {	"Predicate":"sunny",	"Class":"physical"	},
		// {	"Predicate":"smooth",	"Class":"physical"	},
		// {	"Predicate":"sour",	"Class":"physical"	},
		// {	"Predicate":"blunt",	"Class":"physical"	},
		// {	"Predicate":"solid",	"Class":"physical"	},
		// {	"Predicate":"spicy",	"Class":"physical"	},
		// {	"Predicate":"frozen",	"Class":"physical"	},
		// {	"Predicate":"lacy",	"Class":"physical"	},
		// {	"Predicate":"dried",	"Class":"physical"	},
		// {	"Predicate":"dirty",	"Class":"physical"	},
		// {	"Predicate":"humid",	"Class":"physical"	},
		// {	"Predicate":"curly",	"Class":"physical"	},
		// {	"Predicate":"dusty",	"Class":"physical"	},
		// {	"Predicate":"quiet",	"Class":"physical"	},
		{	"Predicate":"circular",	"Class":"shape"	},
		{	"Predicate":"square",	"Class":"shape"	}, // GREG ADDED THIS ONE
		{	"Predicate":"fast",	"Class":"speed"	},
		{	"Predicate":"speedy",	"Class":"speed"	},
		{	"Predicate":"slow",	"Class":"speed"	},
		{	"Predicate":"everyday",	"Class":"temporal"	},
		{	"Predicate":"historical",	"Class":"temporal"	},
		{	"Predicate":"current",	"Class":"temporal"	},
		{	"Predicate":"daily",	"Class":"temporal"	},
		{	"Predicate":"favorite",	"Class":"value"	},
		{	"Predicate":"good",	"Class":"value"	},
		{	"Predicate":"fun",	"Class":"value"	},
		{	"Predicate":"prime",	"Class":"value"	},
		{	"Predicate":"inappropriate",	"Class":"value"	},
		{	"Predicate":"popular",	"Class":"value"	},
		{	"Predicate":"prestigious",	"Class":"value"	},
		{	"Predicate":"lavish",	"Class":"value"	},
		// {	"Predicate":"ugly",	"Class":"value"	},
		// {	"Predicate":"ideal",	"Class":"value"	},
		// {	"Predicate":"detailed",	"Class":"value"	},
		// {	"Predicate":"bad",	"Class":"value"	},
		// {	"Predicate":"simple",	"Class":"value"	},
		// {	"Predicate":"negative",	"Class":"value"	},
		// {	"Predicate":"great",	"Class":"value"	},
		// {	"Predicate":"practical",	"Class":"value"	},
		// {	"Predicate":"special",	"Class":"value"	},
		// {	"Predicate":"normal",	"Class":"value"	},
		// {	"Predicate":"elaborate",	"Class":"value"	},
		// {	"Predicate":"fancy",	"Class":"value"	},
		// {	"Predicate":"genuine",	"Class":"value"	},
		// {	"Predicate":"exciting",	"Class":"value"	},
		// {	"Predicate":"important",	"Class":"value"	},
		// {	"Predicate":"pleasant",	"Class":"value"	},
		// {	"Predicate":"positive",	"Class":"value"	},
		// {	"Predicate":"safe",	"Class":"value"	},
		// {	"Predicate":"excellent",	"Class":"value"	},
		// {	"Predicate":"wonderful",	"Class":"value"	},
		// {	"Predicate":"reasonable",	"Class":"value"	},
		// {	"Predicate":"quaint",	"Class":"value"	},
		// {	"Predicate":"fantastic",	"Class":"value"	},
		// {	"Predicate":"easy",	"Class":"value"	},
		// {	"Predicate":"incredible",	"Class":"value"	},
		// {	"Predicate":"cute",	"Class":"value"	},
		// {	"Predicate":"effortless",	"Class":"value"	},
		// {	"Predicate":"convenient",	"Class":"value"	},
		// {	"Predicate":"strange",	"Class":"value"	},
		// {	"Predicate":"lovely",	"Class":"value"	},
		// {	"Predicate":"sophisticated",	"Class":"value"	},
		// {	"Predicate":"silly",	"Class":"value"	},
		// {	"Predicate":"beautiful",	"Class":"value"	},
		// {	"Predicate":"acceptable",	"Class":"value"	},
		// {	"Predicate":"best",	"Class":"value"	},
		// {	"Predicate":"plain",	"Class":"value"	},
		// {	"Predicate":"gorgeous",	"Class":"value"	},
		// {	"Predicate":"gross",	"Class":"value"	},
		// {	"Predicate":"ludicrous",	"Class":"value"	},
		// {	"Predicate":"interesting",	"Class":"value"	},
		// {	"Predicate":"useful",	"Class":"value"	},
		{	"Predicate":"specific",	"Class":"X"	},
		{	"Predicate":"spare",	"Class":"X"	},
		{	"Predicate":"same",	"Class":"X"	},
		{	"Predicate":"different",	"Class":"X"	},
		{	"Predicate":"simplified",	"Class":"X"	},
		{	"Predicate":"mixed",	"Class":"X"	},
		{	"Predicate":"individual",	"Class":"X"	},
		{	"Predicate":"designated",	"Class":"X"	},
		// {	"Predicate":"additional",	"Class":"X"	},
		// {	"Predicate":"middle",	"Class":"X"	},
		// {	"Predicate":"immediate",	"Class":"X"	},
		// {	"Predicate":"whole",	"Class":"X"	},
		// {	"Predicate":"unique",	"Class":"X"	},
		// {	"Predicate":"front",	"Class":"X"	},
		// {	"Predicate":"potential",	"Class":"X"	},
		// {	"Predicate":"obvious",	"Class":"X"	},
		// {	"Predicate":"last",	"Class":"X"	},
		// {	"Predicate":"oddball",	"Class":"X"	},
		// {	"Predicate":"random",	"Class":"X"	},
		// {	"Predicate":"similar",	"Class":"X"	},
		// {	"Predicate":"original",	"Class":"X"	},
		// {	"Predicate":"standardized",	"Class":"X"	},
		// {	"Predicate":"actual",	"Class":"X"	},
		// {	"Predicate":"previous",	"Class":"X"	},
		// {	"Predicate":"present",	"Class":"X"	},
		// {	"Predicate":"second",	"Class":"X"	},
		// {	"Predicate":"prevailing",	"Class":"X"	},
		// {	"Predicate":"extra",	"Class":"X"	},
		// {	"Predicate":"token",	"Class":"X"	},
		// {	"Predicate":"complete",	"Class":"X"	}
];
// 78 adjectives

var nouns = [
		{"Noun":"apple", "NounClass":"food"},
		{"Noun":"banana", "NounClass":"food"},
		{"Noun":"carrot", "NounClass":"food"},
		{"Noun":"cheese", "NounClass":"food"},
		{"Noun":"tomato", "NounClass":"food"},								
		{"Noun":"chair", "NounClass":"furniture"},								
		{"Noun":"couch", "NounClass":"furniture"},								
		{"Noun":"fan", "NounClass":"furniture"},								
		{"Noun":"TV", "NounClass":"furniture"},								
		{"Noun":"desk", "NounClass":"furniture"}								
];

var stimuli =  makeStims();

function makeStims() {
	stims = [];

	for (i=0; i<classes.length; i++) {
		function filterClass(obj) {
		  if (obj.Class === classes[i]) {
		    return true;
		  } else {
		    return false;
		  }
		};
		function filterNotClass(obj) {
		  if (obj.Class != classes[i]) {
		    return true;
		  } else {
		    return false;
		  }
		}
		adjs = adjectives.filter(filterClass);
		other_adjs = adjectives.filter(filterNotClass);
		for (j=0; j<adjs.length; j++) {
			pred1 = adjs[j];
			for (k=0; k<other_adjs.length; k++) {
				pred2 = other_adjs[k];
				reverse = pred1.Predicate+pred2.Predicate;
				function filterReverse(obj) {
				  if (obj.Reverse === reverse) {
				    return true;
				  } else {
				    return false;
				  }
				}
				if (stims.filter(filterReverse).length<1) {
					for (l=0; l<nouns.length; l++) {
						noun = nouns[l];
						stims.push(
							{
								"Predicate1":pred1.Predicate,
								"Class1":pred1.Class,	
								"Predicate2":pred2.Predicate,
								"Class2":pred2.Class,
								"Reverse":pred2.Predicate+pred1.Predicate,
								"Noun":noun.Noun,
								"NounClass":noun.NounClass	
							}			
						);	
					}
				}
			}
		}
	}
		
	return stims;
	
}


// function filterAdj1(obj) {
// 		  if (obj.Predicate1 === "blue") {
// 		    return true;
// 		  } else {
// 		    return false;
// 		  }
// 		};

// function filterAdj2(obj) {
// 		  if (obj.Predicate2 === "blue") {
// 		    return true;
// 		  } else {
// 		    return false;
// 		  }
// 		};