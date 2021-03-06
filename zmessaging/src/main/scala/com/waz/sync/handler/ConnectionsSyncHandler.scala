/*
 * Wire
 * Copyright (C) 2016 Wire Swiss GmbH
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package com.waz.sync.handler

import com.waz.ZLog._
import com.waz.ZLog.ImplicitTag._
import com.waz.content.UsersStorage
import com.waz.model.UserData.ConnectionStatus
import com.waz.model.UserId
import com.waz.service.{ConnectionServiceImpl, EventPipeline}
import com.waz.sync.SyncResult
import com.waz.sync.client.ConnectionsClient
import com.waz.threading.Threading
import com.waz.utils.events.EventContext

import scala.concurrent.Future

class ConnectionsSyncHandler(usersStorage:      UsersStorage,
                             connectionService: ConnectionServiceImpl,
                             connectionsClient: ConnectionsClient) {

  import Threading.Implicits.Background
  private implicit val ec = EventContext.Global

  def syncConnections(): Future[SyncResult] = {
    connectionsClient.loadConnections().future flatMap {
      case Left(error) =>
        warn("syncConnections failed")
        Future.successful(SyncResult(error))
      case Right(connections) =>
        connectionService.handleUserConnectionEvents(connections).map(_ => SyncResult.Success)
    }
  }

  def postConnection(userId: UserId, name: String, message: String): Future[SyncResult] =
    connectionsClient.createConnection(userId, name, message).future flatMap {
      case Right(event) =>
        verbose(s"postConnection($userId) success: $event")
        connectionService.handleUserConnectionEvents(Seq(event)).map(_ => SyncResult.Success)

      case Left(error) =>
        warn("postConnection failed")
        Future.successful(SyncResult(error))
    }

  def postConnectionStatus(userId: UserId, status: Option[ConnectionStatus]): Future[SyncResult] = usersStorage.get(userId) flatMap {
    case Some(user) => connectionsClient.updateConnection(userId, status getOrElse user.connection).future flatMap {
      case Right(Some(event)) =>
        connectionService.handleUserConnectionEvents(Seq(event)).map(_ => SyncResult.Success)

      case Right(None) =>
        warn("postConnectionStatus was successful, but didn't return an event, no change")
        Future.successful(SyncResult.Success)

      case Left(error) =>
        // FIXME: handle 'bad-conn-update' response, it's possible that there is some race condition and the state that
        // we are trying to use, is no longer valid, we should sync correct state and update local db
        // for example: other user might have already cancelled connection request that we are trying to accept
        Future.successful(SyncResult(error))
    }

    case None =>
      error(s"No user found for id: $userId")
      Future.successful(SyncResult.failed())
  }
}
