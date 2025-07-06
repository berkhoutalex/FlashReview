const express = require('express')
const path = require('path')
const app = express()
const port = process.env.PORT || 3000

// Serve static files from the current directory
app.use(express.static(__dirname))

// Always send the index.html for any request
app.get('*', (req, res) => {
  res.sendFile(path.resolve(__dirname, 'index.html'))
})

app.listen(port, () => {
  console.log(`Server running at http://localhost:${port}`)
  console.log('Press Ctrl+C to quit')
})
