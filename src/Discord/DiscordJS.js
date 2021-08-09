const Discord = require('discord.js');
const Intent = Discord.Intents.FLAGS

exports.newClient = () => { 
	return new Discord.Client(
		{ intents: [Intent.GUILDS, Intent.GUILD_MESSAGES, Intent.GUILD_MESSAGE_REACTIONS, Intent.DIRECT_MESSAGES, Intent.DIRECT_MESSAGE_REACTIONS] }
	) 
}

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
