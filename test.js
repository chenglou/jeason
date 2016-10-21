/**
 * Copyright 2004-present Facebook. All Rights Reserved.
 *
 * @providesModule MessengerThreadlistRowActions.react
 * @typechecks
 * @flow
 * @fbt {"project": "messenger.com"}
 */

'use strict';

const MessengerActions = require('MessengerActions');
const MessengerDeleteDialog = require('MessengerDeleteDialog.react');
const MessengerMarkSpamDialog = require('MessengerMarkSpamDialog.react');
const MessengerMenu = require('MessengerMenu.react');
const MessengerMuteDialog = require('MessengerMuteDialog.react');
const MessengerPopoverMenu = require('MessengerPopoverMenu.react');
const React = require('React');

const cx = require('cx');
const fbt = require('fbt');
const joinClasses = require('joinClasses');

const MessengerMenuItem = MessengerMenu.Item;
const MessengerMenuSeparator = MessengerMenu.Separator;
const {PropTypes} = React;

class MessengerThreadlistRowActions extends React.PureComponent {
  props: {
    className?: string,
    isMuted: boolean,
    isOpen: boolean,
    isUnread: boolean,
    onArchive: Function,
    onDelete: Function,
    onLeaveGroup?: Function,
    onMarkRead: Function,
    onMarkSpam: Function,
    onMarkUnread: Function,
    onMute: Function,
    onRename?: Function,
    onToggle: Function,
    onUnmute: Function,
    showLeaveGroup: boolean,
    showMute: boolean,
    showRename?: boolean,
  };

  static propTypes = {
    isMuted: PropTypes.bool.isRequired,
    isOpen: PropTypes.bool.isRequired,
    isUnread: PropTypes.bool.isRequired,
    onArchive: PropTypes.func.isRequired,
    onDelete: PropTypes.func.isRequired,
    onLeaveGroup: PropTypes.func,
    onMarkRead: PropTypes.func.isRequired,
    onMarkSpam: PropTypes.func.isRequired,
    onMarkUnread: PropTypes.func.isRequired,
    onMute: PropTypes.func.isRequired,
    onRename: PropTypes.func,
    onToggle: PropTypes.func.isRequired,
    onUnmute: PropTypes.func.isRequired,
    showLeaveGroup: PropTypes.bool.isRequired,
    showMute: PropTypes.bool.isRequired,
    showRename: PropTypes.bool,
  };

  render(): ReactElement<any> {
    const {
      className,
      isOpen,
      isUnread,
      onLeaveGroup,
      onToggle,
      showLeaveGroup,
      showMute,
      showRename,
    } = this.props;

    const menu = (
      <MessengerMenu>
        {showMute &&
          <MessengerMenuItem
            label={this.props.isMuted
              ? fbt('Unmute', 'link to unmute a thread')
              : fbt('Mute', 'link to mute a thread')}
            onclick={this._handleMuteClick}
          />
        }
        {showRename &&
          <MessengerMenuItem
            label={fbt('Rename', 'link to rename a thread')}
            onclick={this.props.onRename}
          />
        }
        {(showMute || showRename) && <MessengerMenuSeparator />}

        {showLeaveGroup &&
          <MessengerMenuItem
            label={fbt('Leave Group', 'Leave this conversation')}
            onclick={onLeaveGroup}
          />}
        <MessengerMenuItem
          label={fbt('Archive', 'link to archive a thread')}
          onclick={this.props.onArchive}
        />
        <MessengerMenuItem
          label={fbt('Delete', 'link to delete a thread')}
          onclick={this._handleDeleteClick}
        />
        <MessengerMenuSeparator />
        <MessengerMenuItem
          label={isUnread
            ? fbt('Mark as Read', 'link to mark a thread as read')
            : fbt('Mark as Unread', 'link to mark a thread as unread')}
          onclick={this._handleMarkReadClick}
        />
        <MessengerMenuItem
          label={fbt('Mark as Spam', 'link to mark a thread as spam')}
          onclick={this._handleMarkSpamClick}
        />
      </MessengerMenu>
    );
    return (
      <div className={cx('messengerThreadlistRowActions/root')}>
        <MessengerPopoverMenu
          disableArrowKeyActivation={true}
          isOpen={isOpen}
          menu={menu}
          onHide={() => onToggle(false)}
          onShow={() => onToggle(true)}>
          <div
            aria-label={fbt('Conversation actions', 'Conversation actions')}
            className={joinClasses(
              className,
              cx({
                'messengerThreadlistRowActions/button': true,
                'messengerThreadlistRowActions/open': isOpen,
              })
            )}
            onClick={this._handleClick}
            role="button"
            ref="button"
            tabIndex={-1}
          />
        </MessengerPopoverMenu>
      </div>
    );
  }

  _handleClick = (e: SyntheticMouseEvent): void => {
    e.preventDefault();
    e.stopPropagation();
  };

  _handleDeleteClick = (): void => {
    MessengerActions.showDialog(MessengerDeleteDialog, {
      onArchive: this.props.onArchive,
      onDelete: this.props.onDelete,
      onToggle: this._handleDialogToggle,
    });
  };

  _handleMuteClick = (): void => {
    if (this.props.isMuted) {
      this.props.onUnmute && this.props.onUnmute();
    } else {
      MessengerActions.showDialog(MessengerMuteDialog, {
        onMute: this.props.onMute,
        onToggle: this._handleDialogToggle,
      });
    }
  };

  _handleMarkReadClick = (): void => {
    if (this.props.isUnread) {
      this.props.onMarkRead && this.props.onMarkRead();
    } else {
      this.props.onMarkUnread && this.props.onMarkUnread();
    }
  };

  _handleMarkSpamClick = (): void => {
    MessengerActions.showDialog(MessengerMarkSpamDialog, {
      onMarkSpam: this.props.onMarkSpam,
      onToggle: this._handleDialogToggle,
    });
  };

  _handleDialogToggle = (isShown: boolean): void => {
    if (!isShown) {
      MessengerActions.hideDialog();
    }
  };
}

module.exports = MessengerThreadlistRowActions;
