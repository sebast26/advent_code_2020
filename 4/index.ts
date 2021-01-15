import * as lineReader from 'line-reader'

let lineRe = /(\w+:\S+)/gm
let keyValueRe = /^(\w+):(\S+)$/

function addNewEntry(input: Array<Map<string, string>>): void {
    input.push(new Map())
}

function addKeyValuesToEntry(input: Array<Map<string, string>>, groups: Array<string>): void {
    for (var keyValue of groups) {
        let keyValueGroups = keyValue.match(keyValueRe)
        if (keyValueGroups == null) continue
        input[input.length - 1].set(keyValueGroups[1], keyValueGroups[2])
    }
}


function readLines(filename: string, processLine: (line: string) => Promise<void>): Promise<void> {
    return new Promise((resolve, reject) => {
      lineReader.eachLine(filename, (line, last, callback) => {
        if (!callback) throw new Error('panic');
        processLine(line)
          .then(() => last ? resolve() : callback())
          .catch(reject);
      });
    });
  }
  
async function resolve4(): Promise<void> {
    let input: Array<Map<string, string>> = [new Map()]
    await readLines('4.input', async (line) => {
        let lineGroups = line.match(lineRe)
        if (lineGroups == null) {
            addNewEntry(input)
            return
        }
        addKeyValuesToEntry(input, lineGroups)
    });
    
    let result4a = input.filter((data) => 
        data.has('byr') && data.has('iyr') && data.has('eyr') && data.has('hgt') 
        && data.has('hcl') && data.has('ecl') && data.has('pid'))
    console.log('4a=' + result4a.length)

    function validBYR(val: string): boolean {
        return /^\d{4}$/.test(val) && parseInt(val) >= 1920 && parseInt(val) <= 2002
    }

    function validIYR(val: string): boolean {
        return /^\d{4}$/.test(val) && parseInt(val) >= 2010 && parseInt(val) <= 2020
    }

    function validEYR(val: string): boolean {
        return /^\d{4}$/.test(val) && parseInt(val) >= 2020 && parseInt(val) <= 2030
    }

    function validHgt(val: string): boolean {
        let groups = val.match(/^(\d{2,3})(cm|in)$/)
        if (groups == null) return false
        if (groups[2] == 'cm') {
            return parseInt(groups[1]) >= 150 && parseInt(groups[1]) <= 193
        }
        return parseInt(groups[1]) >= 59 && parseInt(groups[1]) <= 76
    }

    function validHcl(val: string): boolean {
        return /^#[a-f0-9]{6}$/.test(val)
    }

    function validEcl(val: string): boolean {
        return /^amb|blu|brn|gry|grn|hzl|oth$/.test(val)
    }

    function validPid(val: string): boolean {
        return /^[0-9]{9}$/.test(val)
    }

    let result4b = result4a.filter((data) =>
        validBYR(data.get('byr')!) && validIYR(data.get('iyr')!) && 
        validEYR(data.get('eyr')!) && validHgt(data.get('hgt')!) &&
        validHcl(data.get('hcl')!) && validEcl(data.get('ecl')!) &&
        validPid(data.get('pid')!)
    )
    console.log('4b=' + result4b.length)
}
  
resolve4();
