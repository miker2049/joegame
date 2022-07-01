import { render } from 'preact'
import { App } from './app'
import './index.css'

const context = new AudioContext()
render(<App context={context}/>, document.getElementById('app')!)
