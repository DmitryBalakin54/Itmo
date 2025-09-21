package solution

/**
 * @author Dmitry Balakin
 */
class DijkstraProcessImpl(private val env: Environment) : DijkstraProcess {
    sealed class NetworkPacket(val pathLength: Long) : java.io.Serializable {
        class DistanceUpdate(length: Long) : NetworkPacket(length)
        class Confirmation(accepted: Boolean) : NetworkPacket(if (accepted) 1 else -1)
        class TerminationNotice(length: Long) : NetworkPacket(length)

        companion object {
            val ACCEPT = Confirmation(true)
            val REJECT = Confirmation(false)
        }
    }

    private var predecessorId = -1
    private var activeChildren = 0
    private var pendingResponses = 0
    private var currentDistance: Long = Long.MAX_VALUE

    override fun onMessage(srcId: Int, message: Any) {
        when (val packet = message as? NetworkPacket) {
            is NetworkPacket.DistanceUpdate -> handleDistanceUpdate(srcId, packet)
            is NetworkPacket.Confirmation -> handleConfirmation(packet)
            is NetworkPacket.TerminationNotice -> handleTerminationNotice()
            else -> return
        }
    }

    private fun handleDistanceUpdate(sourceId: Int, packet: NetworkPacket.DistanceUpdate) {
        if (packet.pathLength >= currentDistance) {
            env.send(sourceId, NetworkPacket.REJECT)
            return
        }

        currentDistance = packet.pathLength

        if (propagateDistanceUpdate()) {
            when {
                predecessorId == -1 -> {
                    predecessorId = sourceId
                    env.send(sourceId, NetworkPacket.ACCEPT)
                }
                else -> env.send(sourceId, NetworkPacket.REJECT)
            }
        } else {
            env.send(sourceId, NetworkPacket.REJECT)
        }
    }

    private fun handleConfirmation(packet: NetworkPacket.Confirmation) {
        pendingResponses--
        if (packet == NetworkPacket.ACCEPT) {
            activeChildren++
        } else {
            checkCompletionConditions()
        }
    }

    private fun handleTerminationNotice() {
        activeChildren--
        checkCompletionConditions()
    }

    private fun checkCompletionConditions() {
        if (activeChildren == 0 && pendingResponses == 0) {
            if (predecessorId == -1) {
                env.finishExecution()
            } else {
                env.send(predecessorId, NetworkPacket.TerminationNotice(currentDistance))
                predecessorId = -1
            }
        }
    }

    override val distance: Long?
        get() = currentDistance.takeIf { it != Long.MAX_VALUE }

    override fun onComputationStart() {
        currentDistance = 0
        propagateDistanceUpdate()
        checkCompletionConditions()
    }

    private fun propagateDistanceUpdate(): Boolean {
        var updatesSent = false

        env.neighbours.forEach { (nodeId, edgeWeight) ->
            if (nodeId != env.processId) {
                pendingResponses++
                updatesSent = true
                env.send(nodeId, NetworkPacket.DistanceUpdate(currentDistance + edgeWeight))
            }
        }

        return updatesSent
    }
}