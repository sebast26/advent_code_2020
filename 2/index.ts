import lineReader from 'line-reader';

let re = /^(\d+)-(\d+)\s(\w{1}):\s(\w+)$/

console.log('let input = [')
lineReader.eachLine('2.input', function(line: string) {
    var groups = line.match(re)
    if (groups == null) {
        console.log("cannot match")
        return
    }
    console.log('(' + groups[1] + ', ' + groups[2] + ', \'' + groups[3] + '\', "' + groups[4] + '"),')
})