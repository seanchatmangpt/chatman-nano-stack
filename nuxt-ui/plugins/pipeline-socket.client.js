// 🔌 ULTRATHINK SWARM 80/20: Nuxt Pipeline WebSocket Plugin
// JavaScript implementation for Phoenix channel connection

import { Socket } from 'phoenix'

export default defineNuxtPlugin((nuxtApp) => {
  // Create Phoenix socket connection
  const socket = new Socket('/socket', {
    params: {
      token: 'nuxt-ui-client'
    }
  })
  
  // Pipeline channel reference
  let channel = null
  
  // Connection state
  const connectionState = reactive({
    isConnected: false,
    isConnecting: false,
    error: null
  })
  
  // Connect to pipeline channel
  const connect = async () => {
    if (connectionState.isConnected || connectionState.isConnecting) {
      return channel
    }
    
    connectionState.isConnecting = true
    connectionState.error = null
    
    try {
      // Connect socket
      socket.connect()
      
      // Join pipeline channel
      channel = socket.channel('pipeline:ui', {})
      
      return new Promise((resolve, reject) => {
        channel.join()
          .receive('ok', (resp) => {
            console.log('✅ Connected to pipeline channel', resp)
            connectionState.isConnected = true
            connectionState.isConnecting = false
            resolve(channel)
          })
          .receive('error', (resp) => {
            console.error('❌ Failed to join pipeline channel', resp)
            connectionState.error = resp
            connectionState.isConnecting = false
            reject(new Error('Failed to join channel'))
          })
          .receive('timeout', () => {
            console.error('⏱️ Connection timeout')
            connectionState.error = 'Connection timeout'
            connectionState.isConnecting = false
            reject(new Error('Connection timeout'))
          })
      })
    } catch (error) {
      connectionState.isConnecting = false
      connectionState.error = error.message
      throw error
    }
  }
  
  // Disconnect from channel
  const disconnect = () => {
    if (channel) {
      channel.leave()
      channel = null
    }
    
    if (socket) {
      socket.disconnect()
    }
    
    connectionState.isConnected = false
    connectionState.error = null
  }
  
  // Auto-reconnect on disconnect
  socket.onError(() => {
    console.warn('🔌 Socket error, attempting reconnect...')
    connectionState.isConnected = false
  })
  
  socket.onClose(() => {
    console.warn('🔌 Socket closed')
    connectionState.isConnected = false
  })
  
  // Provide socket functionality
  nuxtApp.provide('socket', {
    socket,
    channel,
    connect,
    disconnect,
    connectionState
  })
  
  // Auto-connect when app starts
  if (process.client) {
    onNuxtReady(() => {
      connect().catch(error => {
        console.error('Failed to auto-connect:', error)
      })
    })
  }
  
  // Clean up on app unmount
  nuxtApp.hook('app:unmounted', () => {
    disconnect()
  })
})