package mutex

import java.util.concurrent.locks.ReentrantLock

/**
 * @author Dmitry Balakin
 */
class ProcessImpl(private val env: Environment) : Process {
    private companion object {
        const val IDLE_STATE = 0
        const val REQUESTING_STATE = 1
        const val ACTIVE_STATE = 2

        const val RESOURCE_TRANSFER_MSG = 1
        const val RESOURCE_REQUEST_MSG = 2
    }

    private class ResourceTracker(private val processCount: Int, myId: Int) {
        private val resourceOwned = BooleanArray(processCount + 1)
        private val resourceState = BooleanArray(processCount + 1)
        private val canRequestResource = BooleanArray(processCount + 1)

        init {
            for (i in 1..processCount) {
                resourceOwned[i] = i <= myId
                resourceState[i] = true
                canRequestResource[i] = i > myId
            }
        }

        fun isOwned(pid: Int) = resourceOwned[pid]
        fun isClean(pid: Int) = resourceState[pid]
        fun canRequest(pid: Int) = canRequestResource[pid]

        fun setOwned(pid: Int, value: Boolean) { resourceOwned[pid] = value }
        fun setClean(pid: Int, value: Boolean) { resourceState[pid] = value }
        fun setCanRequest(pid: Int, value: Boolean) { canRequestResource[pid] = value }

        fun checkAllResources(): Boolean {
            for (i in 1..processCount) {
                if (!resourceOwned[i]) return false
            }
            return true
        }
    }

    private val resources = ResourceTracker(env.nProcesses, env.processId)
    private var processState = IDLE_STATE
    private val accessLock = ReentrantLock()

    override fun onMessage(srcId: Int, message: Message) {
        accessLock.lock()
        try {
            message.parse {
                when (readInt()) {
                    RESOURCE_REQUEST_MSG -> processResourceRequest(srcId)
                    RESOURCE_TRANSFER_MSG -> processResourceTransfer(srcId)
                }
            }
        } finally {
            accessLock.unlock()
        }
    }

    private fun processResourceRequest(senderId: Int) {
        resources.setCanRequest(senderId, true)
        if (processState != ACTIVE_STATE &&
            resources.isOwned(senderId) &&
            resources.isClean(senderId)) {

            sendResourceMessage(senderId, RESOURCE_TRANSFER_MSG)
            resources.setOwned(senderId, false)

            if (processState == REQUESTING_STATE) {
                sendResourceMessage(senderId, RESOURCE_REQUEST_MSG)
                resources.setCanRequest(senderId, false)
            }
        }
    }

    private fun processResourceTransfer(senderId: Int) {
        resources.setOwned(senderId, true)
        resources.setClean(senderId, false)

        if (resources.checkAllResources()) {
            processState = ACTIVE_STATE
            env.locked()
        }
    }

    override fun onLockRequest() {
        accessLock.lock()
        try {
            processState = REQUESTING_STATE
            if (resources.checkAllResources()) {
                processState = ACTIVE_STATE
                env.locked()
                return
            }

            for (i in 1..env.nProcesses) {
                if (resources.canRequest(i) && !resources.isOwned(i)) {
                    sendResourceMessage(i, RESOURCE_REQUEST_MSG)
                    resources.setCanRequest(i, false)
                }
            }
        } finally {
            accessLock.unlock()
        }
    }

    override fun onUnlockRequest() {
        accessLock.lock()
        try {
            processState = IDLE_STATE
            env.unlocked()

            for (i in 1..env.nProcesses) {
                resources.setClean(i, true)
                if (resources.canRequest(i)) {
                    sendResourceMessage(i, RESOURCE_TRANSFER_MSG)
                    resources.setOwned(i, false)
                }
            }
        } finally {
            accessLock.unlock()
        }
    }

    private fun sendResourceMessage(pid: Int, messageType: Int) {
        env.send(pid) {
            writeInt(messageType)
        }
    }
}