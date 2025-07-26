export default defineNuxtPlugin(() => {
  // WebSocket plugin for client-side only
  if (process.client) {
    // Add any global WebSocket configuration here
    console.log('WebSocket plugin initialized on client')
    
    // You can add global WebSocket utilities here
    return {
      provide: {
        wsUtils: {
          // Helper to check WebSocket support
          isSupported: () => typeof WebSocket !== 'undefined',
          
          // Helper to create WebSocket connection with reconnection logic
          createConnection: (url, options = {}) => {
            const {
              maxReconnectAttempts = 5,
              reconnectInterval = 5000,
              onConnect = () => {},
              onDisconnect = () => {},
              onError = () => {},
              onMessage = () => {}
            } = options
            
            let reconnectAttempts = 0
            let socket = null
            
            const connect = () => {
              try {
                socket = new WebSocket(url)
                
                socket.onopen = (event) => {
                  console.log('WebSocket connected')
                  reconnectAttempts = 0
                  onConnect(event)
                }
                
                socket.onclose = (event) => {
                  console.log('WebSocket disconnected')
                  onDisconnect(event)
                  
                  // Attempt reconnection
                  if (reconnectAttempts < maxReconnectAttempts) {
                    reconnectAttempts++
                    console.log(`Reconnection attempt ${reconnectAttempts}/${maxReconnectAttempts}`)
                    setTimeout(connect, reconnectInterval)
                  }
                }
                
                socket.onerror = (error) => {
                  console.error('WebSocket error:', error)
                  onError(error)
                }
                
                socket.onmessage = (event) => {
                  try {
                    const data = JSON.parse(event.data)
                    onMessage(data)
                  } catch (e) {
                    console.error('Failed to parse WebSocket message:', e)
                  }
                }
                
              } catch (error) {
                console.error('Failed to create WebSocket connection:', error)
                onError(error)
              }
            }
            
            connect()
            
            return {
              socket: () => socket,
              send: (data) => {
                if (socket && socket.readyState === WebSocket.OPEN) {
                  socket.send(typeof data === 'string' ? data : JSON.stringify(data))
                  return true
                }
                return false
              },
              close: () => {
                if (socket) {
                  socket.close()
                  socket = null
                }
              }
            }
          }
        }
      }
    }
  }
})