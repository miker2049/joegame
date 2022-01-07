import fs from 'fs/promises'
import path from 'path'



const LETTERS = [
    //A-Z
    65,66,67,68,69,70,71,72,73,74,
    75,76,77,78,79,80,81,82,83,84,
    85,86,87,88,89,90,
    //a-z
    97,98,99,100,101,102,103,104,105,106,
    107,108,109,110,111,112,113,114,115,116,
    117,118,119,120,121,122
];

function char(n: number): string {return String.fromCharCode(n)}

(async function(){
    let template
    const templatePath = process.argv[2]
    try {
        template = await fs.readFile(templatePath, 'utf-8')

    } catch (err) {
        // Do something
        throw Error('can\'t get template')
    }

    // the world will be nxn maps big
    // right now hardcoded for file naming convention
    const n = parseInt(process.argv[3]) ?? 10
    const prefix =   process.argv[4] + '_' ?? 'world_'
    const outPath = path.dirname(templatePath)

    for (let y = 0; y < n; y++) {
        for (let x = 0; x < n; x++) {
            const name = outPath +'/'+ prefix + char(LETTERS[x]) +
                char(LETTERS[y]) + '.json'
            console.log(name, x,y)
            await fs.writeFile(name,template)
        }
    }

})()
