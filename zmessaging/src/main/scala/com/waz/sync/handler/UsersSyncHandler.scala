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
import com.waz.api.impl.ErrorResponse
import com.waz.content.UsersStorage
import com.waz.model._
import com.waz.service.UserService
import com.waz.service.assets.AssetService
import com.waz.service.images.ImageAssetGenerator
import com.waz.sync.SyncResult
import com.waz.sync.client.AssetClient.Retention
import com.waz.sync.client.UsersClient
import com.waz.sync.otr.OtrSyncHandler
import com.waz.threading.Threading
import com.waz.utils.events.EventContext

import scala.concurrent.Future

class UsersSyncHandler(assetSync: AssetSyncHandler,
                       userService: UserService,
                       usersStorage: UsersStorage,
                       assets: AssetService,
                       usersClient: UsersClient,
                       imageGenerator: ImageAssetGenerator,
                       otrSync: OtrSyncHandler) {
  import Threading.Implicits.Background
  private implicit val ec = EventContext.Global

  def syncUsers(ids: UserId*): Future[SyncResult] = usersClient.loadUsers(ids).future flatMap {
    case Right(users) =>
      userService.updateSyncedUsers(users) map { _ => SyncResult.Success }
    case Left(error) =>
      warn(s"load user request failed for: $ids")
      Future.successful(SyncResult(error))
  }

  def syncSelfUser(): Future[SyncResult] = usersClient.loadSelf().future flatMap {
    case Right(user) =>
      userService.updateSyncedUsers(IndexedSeq(user)) map { _ => SyncResult.Success }
    case Left(error) =>
      warn(s"load self request failed")
      Future.successful(SyncResult(error))
  }

  def postSelfUser(info: UserInfo): Future[SyncResult] = updatedSelfToSyncResult(usersClient.updateSelf(info))

  def postSelfPicture(): Future[SyncResult] = userService.getSelfUser flatMap {
    case Some(userData) => userData.picture match {
      case Some(assetId)  => postSelfPicture(userData.id, assetId)
      case None           => updatedSelfToSyncResult(usersClient.updateSelf(UserInfo(userData.id, picture = None)))
    }
    case _ => Future.successful(SyncResult.failed())
  }

  def postAvailability(availability: Availability): Future[SyncResult] = {
    verbose(s"postAvailability($availability)")
    otrSync.broadcastMessage(GenericMessage(Uid(), GenericContent.AvailabilityStatus(availability)))
      .map {
        case Left(e) => SyncResult.Failure(Some(e))
        case Right(_) => SyncResult.Success
      }
  }

  private def postSelfPicture(id: UserId, assetId: AssetId): Future[SyncResult] = for {
    Some(asset) <- assets.getAssetData(assetId)
    preview     <- imageGenerator.generateSmallProfile(asset).future
    _           <- assets.mergeOrCreateAsset(preview) //needs to be in storage for other steps to find it
    res         <- assetSync.uploadAssetData(preview.id, public = true, retention = Retention.Eternal).future flatMap {
      case Right(uploadedPreview) =>
        assetSync.uploadAssetData(assetId, public = true, retention = Retention.Eternal).future flatMap {
          case Right(uploaded) => for {
            _     <- assets.getAssetData(assetId)
            res   <- updatedSelfToSyncResult(usersClient.updateSelf(UserInfo(id, picture = Some(Seq(uploadedPreview, uploaded).flatten))))
          } yield res

          case Left(err) =>
            error(s"self picture upload asset $assetId failed: $err")
            Future.successful(SyncResult.failed())
        }
      case Left(err) =>
        warn(s"Failed to upload small profile picture: $err")
        Future.successful(SyncResult.failed())
    }
  } yield res


  def syncConnectedUsers(): Future[SyncResult] = {
    usersStorage.getContactNameParts.future flatMap { cs =>
      usersClient.loadUsers(cs.keys.toSeq)
    } flatMap {
      case Right(users) => userService.updateSyncedUsers(users).map {_ => SyncResult.Success }
      case Left(error) =>
        warn(s"UsersClient.loadUsers failed")
        Future.successful(SyncResult(error))
    }
  }

  def deleteAccount(): Future[SyncResult] = usersClient.deleteAccount() map {
    case Right(()) => SyncResult.Success
    case Left(error) =>
      warn(s"Account deletion failed: $error")
      SyncResult(error)
  }

  private def updatedSelfToSyncResult(updatedSelf: Future[Either[ErrorResponse, Unit]]): Future[SyncResult] =
    updatedSelf map (_.fold[SyncResult](SyncResult(_), _ => SyncResult.Success))
}
