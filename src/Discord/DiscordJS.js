const Discord = require('discord.js');

exports.newClient = () => { return new Discord.Client() }

const once = event => callback => client => () => {
    client.once(event, callback)
    return client
}

exports.onceReady = once("ready")

exports.onMessage = (callback) => (client) => () => {
    client.on("message", x => callback(x)())
}

exports.onMessageUpdate = (callback) => (client) => () => {
    client.on("messageUpdate", (oldMsg, newMsg) => callback(oldMsg)(newMsg)())
}

exports.loginImpl = token => client => () => {  
    return client.login(token).then(() => client)
}

exports.reactImpl = reaction => msg => () => {  
    return msg.react(reaction)
}

exports.getChannelTypeImpl = channel => channel.type

exports.removeAllReactionsImpl = msg => () => {  
    return msg.reactions.removeAll()
}

exports.sendStringImpl = 
  stringMsg => channel => () => channel.send(stringMsg)

exports.createDMChannelImpl = (user) => () => { return user.createDM() } 
