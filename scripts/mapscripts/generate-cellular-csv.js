const size = process.argv[2]
const n = process.argv[4]

function generateCellsArr(size,n){
    let out = []
    let subdivSize = size/n
    let points = []
    for(let i = 0; i< n*n; i++){
        const choice = Math.floor(Math.random()*(subdivSize*subdivSize))
        points.push({
            x: (choice % subdivSize) + ((i%n)*subdivSize),
            y: Math.floor(choice/subdivSize) + (Math.floor(i/n)*subdivSize)
        })
    }
    let maxmin=0
    for (let y = 0; y < size; ++y) {
        out[y] = []
        for (let x = 0; x < size; ++x) {
            let m_dist = 1000
            for (let p =0; p< points.length;p++){
                let dist = distance(points[p].x,x,points[p].y,y)
                // console.log(m_dist,dist)
                m_dist = Math.min(m_dist,dist)
            }
            // if(m_dist>(subdivSize*0.6)){
            //     out[y][x] = 1
            // } else {
            //     out[y][x] = 0
            // }
            out[y][x] = m_dist
            maxmin = Math.max(maxmin,m_dist)
        }
    }

    // out=out.map(row=>row.map(t_dist=>{
    //     return t_dist > maxmin-6.5 ? 1 : 0
    // }))
    // console.log(points)
    // points.forEach(p=>out[p.y][p.x]=1)

    return out

}

function stepFromPoints(arr,pos){


}

function distance(x1,x2,y1,y2){
    return Math.sqrt(Math.pow(x2-x1,2)+Math.pow(y2-y1,2))
}

const out =generateCellsArr(96,8)
// console.log(out)
out.forEach(o=>console.log(o.join(',')))
