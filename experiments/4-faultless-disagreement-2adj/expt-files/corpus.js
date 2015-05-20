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

var nouns = [
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
];

var stimuli =  makeStims();

function makeStims() {
	stims = [];

	while (stims.length < 13) {
		noun = _.sample(nouns);
		pred1 = _.sample(adjectives);
		pred2 = _.sample(adjectives);
		if (pred1.Class!=pred2.Class) {
			stims.push(
				{
					"number":"2",
					"Predicate1":pred1.Predicate,
					"Class1":pred1.Class,	
					"Predicate2":pred2.Predicate,
					"Class2":pred2.Class,			
					"Noun":noun.Noun,
					"NounClass":noun.NounClass,
					"NounAn":noun.NounAn,
					"PredAn":pred1.PredAn
				}			
			);
		}
	};

	for (var i=0; i<13; i++) {
	noun = _.sample(nouns);
	stims.push(
		{
			"number":"1",
			"Predicate1":adjectives[i].Predicate,
			"Class1":adjectives[i].Class,	
			"Predicate2":"NA",
			"Class2":"NA",						
			"Noun":noun.Noun,
			"NounClass":noun.NounClass,
			"NounAn":noun.NounAn,
			"PredAn":pred1.PredAn
		}
		);
	};
		
	return stims;
	
}