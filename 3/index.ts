import * as lineReader from 'line-reader';

console.log('let input = [')
lineReader.eachLine('3.input', function(line: string) {
    console.log('      "' + line + '",')
})