// 40 most frequent noun-predicate combinations in the BNC

//[
//		{"Sentence": "box red", "Predicate": "red", "Noun": "box"},
//		{"Sentence": "box big", "Predicate": "big", "Noun": "box"}
//		]

var adjectives = _.shuffle([
		{"Predicate":"red", "Class":"color","PredAn":"no"},
		{"Predicate":"yellow", "Class":"color","PredAn":"no"},
		{"Predicate":"green", "Class":"color","PredAn":"no"},
		{"Predicate":"blue", "Class":"color","PredAn":"no"},
		{"Predicate":"purple", "Class":"color","PredAn":"no"},
		{"Predicate":"brown", "Class":"color","PredAn":"no"},											
		{"Predicate":"big", "Class":"size","PredAn":"no"},
		{"Predicate":"small", "Class":"size","PredAn":"no"},					
		{"Predicate":"huge", "Class":"size","PredAn":"no"},					
		{"Predicate":"tiny", "Class":"size","PredAn":"no"},					
		{"Predicate":"short", "Class":"size","PredAn":"no"},					
		{"Predicate":"long", "Class":"size","PredAn":"no"},							
		{"Predicate":"wooden", "Class":"material","PredAn":"no"},
		{"Predicate":"plastic", "Class":"material","PredAn":"no"},
		{"Predicate":"metal", "Class":"material","PredAn":"no"},
		{"Predicate":"smooth", "Class":"texture","PredAn":"no"},
		{"Predicate":"hard", "Class":"texture","PredAn":"no"},
		{"Predicate":"soft", "Class":"texture","PredAn":"no"},
		{"Predicate":"old", "Class":"age","PredAn":"yes"},
		{"Predicate":"new", "Class":"age","PredAn":"no"},
		{"Predicate":"rotten", "Class":"age","PredAn":"no"},
		{"Predicate":"fresh", "Class":"age","PredAn":"no"},
		{"Predicate":"good", "Class":"quality","PredAn":"no"},
		{"Predicate":"bad", "Class":"quality","PredAn":"no"},
		{"Predicate":"round", "Class":"shape","PredAn":"no"},						
		{"Predicate":"square", "Class":"shape","PredAn":"no"}
]);

var nouns = _.shuffle([
		{"Noun":"apple", "NounClass":"food","NounAn":"yes"},
		{"Noun":"banana", "NounClass":"food","NounAn":"yes"},
		{"Noun":"carrot", "NounClass":"food","NounAn":"yes"},
		{"Noun":"cheese", "NounClass":"food","NounAn":"no"},
		{"Noun":"tomato", "NounClass":"food","NounAn":"yes"},								
		{"Noun":"chair", "NounClass":"furniture","NounAn":"yes"},								
		{"Noun":"couch", "NounClass":"furniture","NounAn":"yes"},								
		{"Noun":"fan", "NounClass":"furniture","NounAn":"yes"},								
		{"Noun":"TV", "NounClass":"furniture","NounAn":"yes"},								
		{"Noun":"desk", "NounClass":"furniture","NounAn":"yes"}								
]);

var stimuli =  makeStims();

function makeStims() {
	stims = [];

	for (var i=0; i<10; i++) {
	noun = nouns[i];
	pred1 = adjectives[i];
	var subset_adjectives = _.filter(adjectives,function(item){ if (item.Class!=pred1.Class) {return item}; });
	pred2 = _.sample(subset_adjectives);
	stims.push(
		{
			"Predicate1":pred1.Predicate,
			"Class1":pred1.Class,	
			"Predicate2":pred2.Predicate,
			"Class2":pred2.Class,			
			"Noun":noun.Noun,
			"NounClass":noun.NounClass,
			"NounAn":noun.NounAn,
			"PredAn":pred1.PredAn,
			"Encountered":"yes"
		}
		);
	};

	for (var i=10; i<20; i++) {
	noun = nouns[i-10];
	pred1 = adjectives[i];
	var subset_adjectives = _.filter(adjectives,function(item){ if (item.Class!=pred1.Class) {return item}; });
	pred2 = _.sample(subset_adjectives);
	stims.push(
		{
			"Predicate1":pred1.Predicate,
			"Class1":pred1.Class,	
			"Predicate2":pred2.Predicate,
			"Class2":pred2.Class,			
			"Noun":noun.Noun,
			"NounClass":noun.NounClass,
			"NounAn":noun.NounAn,
			"PredAn":pred1.PredAn,
			"Encountered":"no"
		}
		);
	};
		
	return stims;
	
}