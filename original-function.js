var doThingsAndStuff = x => { 
  var temp = []; 
  var temp2; 
  var tmep3; 
  var temp4; 
  while(x.length) { 
    temp2 = x.pop(); 
    if (!temp2) continue; 
    for (tmep3 = 0; tmep3 < temp2.length; tmep3++) 
      if (temp2 && temp2.charCodeAt(tmep3) == 32) { 
        var start = tmep3++ + 1; 
        var doBreak = false; 
        var isFound = false; 
        for (temp4 = 0; temp4 < temp.length; temp4++) { 
          if (doBreak) { 
            break; 
          } 
          tmep3 = start; 
          isFound = false; 
          for (var y = 0; y < temp[temp4].length; y++) { 
            if (!isFound && temp[temp4].charCodeAt(y) == 32) { 
              isFound = true; 
              continue; 
            } else if (!isFound) { 
              continue; 
            }
            if (!temp2.charCodeAt(tmep3) || temp2.charCodeAt(tmep3) < temp[temp4].charCodeAt(y)) { 
              temp4--; 
              if (temp4 < 0) { 
                temp4 = 0; 
              } 
              temp.splice(temp4, 0, temp2); 
              doBreak = true; 
              break; 
            } else if (temp2.charCodeAt(tmep3) == temp[temp4].charCodeAt(y)) { tmep3++; 
              continue; 
            } else { 
              doBreak = true; 
              break; 
            } 
          } 
        } 
        isFound = false; 
        for (temp4 = 0; temp4 < temp.length; temp4++) 
        if (temp2 === temp[temp4]) { 
          isFound = true; 
          break; 
        } 
        // Logic to see if we should add it 
        if (!isFound) { 
          temp.push(temp2); 
        } 
        break 
      } 
  }; 
  while(temp.length) { 
    var newThing = temp.pop(); 
    x.push(newThing); 
  }; 
};

// small bit of glue code so we can run this
// as a script, running the original function with input from stdin
// and output to stdout
var fs = require('fs');
let data = fs.readFileSync(0, 'utf-8');
data = JSON.parse(data);
doThingsAndStuff(data);
process.stdout.write(JSON.stringify(data));
